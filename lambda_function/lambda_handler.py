"""
lambda_handler.py
Authors: William Fitzjohn, Matthew Szurkowski

AWS Lambda function to handle S3 events and execute the R commandline script.

Running locally:
1.  Ensure you have Docker installed.
2.  Build the Docker image with the command:
    docker build -t dpm-lambda -f lambda_function/Dockerfile .
3.  Download the AWS Lambda Runtime Interface Emulator:
    curl -Lo aws-lambda-rie https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/latest/download/aws-lambda-rie
    chmod +x aws-lambda-rie
4.  Run the Docker container with the command:
    docker run \
        -e AWS_ACCESS_KEY_ID=s3_user_key \
        -e AWS_SECRET_ACCESS_KEY=s3_user_secret \
        -e AWS_REGION=ca-central-1 \
        -e AWS_LAMBDA_RUNTIME_API=127.0.0.1:8080 \
        -e RDS_HOST=... \
        -e RDS_DATABASE=... \
        -e RDS_USER=... \
        -e RDS_PASSWORD=... \
        -v "$PWD/aws-lambda-rie:/aws-lambda-rie" \
        --entrypoint /aws-lambda-rie \
        -p 9000:8080 \
        dpm-lambda /usr/local/bin/python -m awslambdaric lambda_handler.lambda_handler
5.  Test the Lambda function locally with a sample S3 event:
    curl -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" -d '{"Records":[{"s3":{"bucket":{"name":"gbads-modelling-inputs"},"object":{"key":"dpm/user_21/new_model_test_zeroMortality.yaml"}}}]}'
"""

from datetime import datetime
import os
import subprocess
import glob
import random
import yaml
import re
import boto3
from rds_adapter import RDSAdapter

def track_model_run(user_id, input_model_name, status, input_file_uri, output_file_uri, time_created, time_completed, input_model_part):
    """
    Helper function to track the model run in the database.
    """
    upsert_sql = """
        INSERT INTO user_models2
            (user_id, model_name, status, file_input_uri, file_output_uri, date_created, date_completed, model_part, time_elapsed)
        VALUES
            (%s, %s, %s, %s, %s, %s, %s, %s, %s)
        ON CONFLICT (user_id, model_name, model_part)
        DO UPDATE SET
            status=EXCLUDED.status,
            file_input_uri=EXCLUDED.file_input_uri,
            file_output_uri=EXCLUDED.file_output_uri,
            date_created=EXCLUDED.date_created,
            date_completed=EXCLUDED.date_completed,
            model_part=EXCLUDED.model_part,
            time_elapsed=EXCLUDED.time_elapsed
    """
    try:
        if time_completed and time_created:
            duration = (time_completed - time_created).total_seconds()
        else:
            duration = None
        with RDSAdapter() as rds_adapter:
            rds_adapter.execute(
                upsert_sql,
                (
                    user_id,
                    input_model_name,
                    status,
                    input_file_uri,
                    output_file_uri,
                    time_created,
                    time_completed,
                    input_model_part,
                    duration
                )
            )
    except Exception as e:
        print(f"Error tracking model run: {e}")

def lambda_handler(event, context):
    """
    AWS Lambda function to process S3 events and run an R script.
    :param event: The event data from S3 trigger
    :param context: The runtime information of the Lambda function
    :return: A dictionary containing the status code and output of the R script
    """
    # Log the received event
    print(f"Received event: {event}")

    # Setting up variables
    input_file_bucket = event['Records'][0]['s3']['bucket']['name']
    input_file_key = event['Records'][0]['s3']['object']['key']
    input_name_part, _ = os.path.splitext(os.path.basename(input_file_key))
    input_parts = input_name_part.split("_")
    input_model_name = "_".join(input_parts[:-1])  # everything except the last part
    input_model_part = input_parts[-1]
    output_bucket = "gbads-modelling-outputs"
    output_prefix = os.path.dirname(input_file_key)
    output_prefix = f"{output_prefix}/" if output_prefix else ""
    local_params_dir = "/tmp/parameters"
    local_params_file = f"{local_params_dir}/{os.path.basename(input_file_key)}"
    model_output_format = "cumulative total"
    model_parallel = "FALSE"
    model_seed = None # Later we read from the YAML file or generate a random seed
    function_dir = os.environ.get("LAMBDA_TASK_ROOT", "/var/task")
    function_script = f"{function_dir}/DPM_CommandLine.R"
    user_id = ""
    if match := re.search(r"/user_([a-zA-Z0-9-]+)/", input_file_key):
        user_id = str(match.group(1))

    # Prevent infinite loops
    if output_bucket == input_file_bucket:
        print("FATAL: Input and output buckets are the same, exiting to prevent infinite loop.")
        return {
            "statusCode": 400,
            "error": "Input and output buckets are the same."
        }
    print(f'Processing parameters file: s3://{input_file_bucket}/{input_file_key}')

    # Create a local directory for parameters
    os.makedirs(local_params_dir, exist_ok=True)

    # Download parameters file from S3
    s3 = boto3.client('s3')
    s3.download_file(input_file_bucket, input_file_key, local_params_file)
    print(f'Downloaded parameters file to {local_params_file}')

    # Read seed value from YAML file, or generate a random 10-digit integer if not present
    try:
        with open(local_params_file, 'r', encoding='utf-8') as f:
            params = yaml.safe_load(f)
            model_seed = str(params.get('seed_value'))
    except Exception as e:
        print(f"Error reading seed_value: {e}")
    if model_seed == "None":
        model_seed = ''.join(random.choices('0123456789', k=9))

    track_model_run(user_id, input_model_name, 'in_progress', f's3://{input_file_bucket}/{input_file_key}', None, datetime.now(), None, input_model_part)

    print('Running R script with args:')
    print(f'       dir: "{local_params_dir}"')
    print(f'      seed: "{model_seed}"')
    print(f'    output: "{model_output_format}"')
    print(f'  parallel: "{model_parallel}"')

    # Run the R script
    time_created = datetime.now()
    function_command = ["Rscript", function_script, local_params_dir, model_seed, model_output_format, model_parallel]
    function_result = subprocess.run(function_command, check=False)
    time_completed = datetime.now()
    print(f'R script return code: {function_result.returncode}')

    # Check if the command was successful
    if function_result.returncode != 0:
        print("R script execution failed")
        track_model_run(user_id, input_model_name, 'error:R script failed', f's3://{input_file_bucket}/{input_file_key}', None, time_created, time_completed, input_model_part)
        return {
            "statusCode": 500,
            "error": "R script execution failed, check lambda logs for details."
        }

    # Ensure the output directory exists
    csv_files = glob.glob(f"{local_params_dir}/*.csv")
    if not csv_files:
        print("No output CSV file found in parameters directory.")
        track_model_run(user_id, input_model_name, 'error:Model produced no output', f's3://{input_file_bucket}/{input_file_key}', None, time_created, time_completed, input_model_part)
        return {
            "statusCode": 500,
            "error": "No output CSV file found."
        }

    # Upload output files to S3
    output_file_uris = []
    for csv_file in csv_files:
        model_output_path = f"{output_prefix}{os.path.basename(csv_file)}"
        print(f'Uploading output file "{csv_file}" to s3://{output_bucket}/{model_output_path}')
        s3.upload_file(csv_file, output_bucket, model_output_path)
        output_file_uris.append(f's3://{output_bucket}/{model_output_path}')

        # Check if the upload was successful
        if not s3.head_object(Bucket=output_bucket, Key=model_output_path):
            print("Failed to upload output file to S3")
            track_model_run(user_id, input_model_name, 'error:Failed to upload output file to S3', f's3://{input_file_bucket}/{input_file_key}', None, time_created, time_completed, input_model_part)
            return {
                "statusCode": 500,
                "error": "Failed to upload output file to S3."
            }

    print('Upload complete and verified.')

    # Record the model run in the database
    track_model_run(user_id, input_model_name, 'success', f's3://{input_file_bucket}/{input_file_key}', ','.join(output_file_uris), time_created, time_completed, input_model_part)

    # Return output
    return {
        "statusCode": 200,
        "body": f"R script executed successfully. Output uploaded to s3://{output_bucket}/{output_prefix}"
    }
