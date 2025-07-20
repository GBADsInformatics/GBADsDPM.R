"""
lambda_handler.py
Authors: William Fitzjohn, Matthew Szurkowski

AWS Lambda function to handle S3 events and execute the R commandline script.

Running locally:
1. Ensure you have Docker installed.
2. Build the Docker image with the command:
   docker build -t dpm-lambda -f lambda_function/Dockerfile .
3. Run the Docker container with the command:
   docker run \
       -e AWS_ACCESS_KEY_ID=s3_user_key \
       -e AWS_SECRET_ACCESS_KEY=s3_user_secret \
       -e AWS_REGION=ca-central-1 \
       -p 9000:8080 dpm-lambda
4. Test the Lambda function locally with a sample S3 event:
   curl -XPOST "http://localhost:9000/2015-03-31/functions/function/invocations" -d '{"Records":[{"s3":{"bucket":{"name":"gbads-modelling-private"},"object":{"key":"params_cattle.yaml"}}}]}'

"""

import os
import subprocess
import boto3

def lambda_handler(event, context):
    """
    AWS Lambda function to process S3 events and run an R script.
    :param event: The event data from S3 trigger
    :param context: The runtime information of the Lambda function
    :return: A dictionary containing the status code and output of the R script
    """
    # Get S3 bucket and key from event
    bucket = event['Records'][0]['s3']['bucket']['name']
    key = event['Records'][0]['s3']['object']['key']

    # Create a local directory for parameters
    local_params_dir = "/tmp/parameters"
    os.makedirs(local_params_dir, exist_ok=True)
    local_path = f"{local_params_dir}/{os.path.basename(key)}"

    # Download file from S3
    s3 = boto3.client('s3')
    s3.download_file(bucket, key, local_path)

    # Example arguments for DPM_CommandLine.R
    seed = "456789423"
    output_format = "summary"
    parallel = "FALSE"

    # Run the R script
    r_script = "/var/task/DPM_CommandLine.R"
    cmd = ["Rscript", r_script, local_params_dir, seed, output_format, parallel]
    result = subprocess.run(cmd, capture_output=True, text=True)

    # Check if the command was successful
    if result.returncode != 0:
        return {
            "statusCode": 500,
            "error": "R script execution failed",
            "stdout": result.stdout,
            "stderr": result.stderr
        }

    # Ensure the output directory exists
    output_bucket = "gbads-modelling-private"
    output_file = f"{local_params_dir}/{os.path.splitext(os.path.basename(key))[0]}_{output_format}.csv"
    output_key = f"model_output/{os.path.basename(output_file)}"

    # Upload the output file to S3
    s3.upload_file(output_file, output_bucket, output_key)

    # Return output
    return {
        "statusCode": 200,
        "stdout": result.stdout,
        "stderr": result.stderr
    }
