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
        -v "$PWD/aws-lambda-rie:/aws-lambda-rie" \
        --entrypoint /aws-lambda-rie \
        -p 9000:8080 \
        dpm-lambda /usr/local/bin/python -m awslambdaric lambda_handler.lambda_handler
5.  Test the Lambda function locally with a sample S3 event:
    curl -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" -d '{"Records":[{"s3":{"bucket":{"name":"gbads-modelling-private"},"object":{"key":"params_cattle.yaml"}}}]}'
"""

import os
import subprocess
import glob
import boto3

def lambda_handler(event, context):
    """
    AWS Lambda function to process S3 events and run an R script.
    :param event: The event data from S3 trigger
    :param context: The runtime information of the Lambda function
    :return: A dictionary containing the status code and output of the R script
    """
    # Get S3 bucket and key from event
    print(f"Received event: {event}")
    bucket = event['Records'][0]['s3']['bucket']['name']
    key = event['Records'][0]['s3']['object']['key']
    print(f'Processing parameters file: s3://{bucket}/{key}')

    # Create a local directory for parameters
    local_params_dir = "/tmp/parameters"
    os.makedirs(local_params_dir, exist_ok=True)
    local_path = f"{local_params_dir}/{os.path.basename(key)}"

    # Download file from S3
    s3 = boto3.client('s3')
    s3.download_file(bucket, key, local_path)
    print(f'Downloaded parameters file to {local_path}')

    # Example arguments for DPM_CommandLine.R
    seed = "456789423"
    output_format = "summary"
    parallel = "FALSE"

    print('Running R script with args:')
    print(f'  dir="{local_params_dir}"')
    print(f'  seed="{seed}"')
    print(f'  output="{output_format}"')
    print(f'  parallel="{parallel}"')

    # Run the R script
    lambda_root = os.environ.get("LAMBDA_TASK_ROOT", "/var/task")
    r_script = f"{lambda_root}/DPM_CommandLine.R"
    cmd = ["Rscript", r_script, local_params_dir, seed, output_format, parallel]
    result = subprocess.run(cmd, check=False)

    print(f'R script return code: {result.returncode}')

    # Check if the command was successful
    if result.returncode != 0:
        print("R script execution failed")
        return {
            "statusCode": 500,
            "error": "R script execution failed, check lambda logs for details."
        }

    # Ensure the output directory exists
    output_bucket = "gbads-modelling-private"
    # local_output_file will be the only .csv file in the local_params_dir
    csv_files = glob.glob(f"{local_params_dir}/*.csv")
    if not csv_files:
        print("No output CSV file found in parameters directory.")
        return {
            "statusCode": 500,
            "error": "No output CSV file found."
        }
    local_output_file = csv_files[0]
    output_key = f"model_output/{os.path.basename(local_output_file)}"
    print(f'Uploading output file "{local_output_file}" to s3://{output_bucket}/{output_key}')

    # Upload the output file to S3
    s3.upload_file(local_output_file, output_bucket, output_key)

    # Check if the upload was successful
    if not s3.head_object(Bucket=output_bucket, Key=output_key):
        print("Failed to upload output file to S3")
        return {
            "statusCode": 500,
            "error": "Failed to upload output file to S3."
        }

    print('Upload complete and verified.')

    # Return output
    return {
        "statusCode": 200,
        "body": f"R script executed successfully. Output uploaded to s3://{output_bucket}/{output_key}"
    }
