#
# Dockerfile for AWS Lambda function using R and Python.
# The model is run in R, and the Lambda function is written in Python.
#

# Use official Python slim image
FROM python:3.13-slim

# Set Lambda working directory
ENV LAMBDA_TASK_ROOT=/var/task
WORKDIR $LAMBDA_TASK_ROOT

# Add R repository for Debian Bookworm
RUN apt-get update && \
    apt install -y --no-install-recommends gnupg2
RUN echo "deb https://cloud.r-project.org/bin/linux/debian bookworm-cran40/" >> /etc/apt/sources.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys B8F25A8A73EACF41

# Install R and dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        cmake \
        r-base \
        r-base-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        git && \
    rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN Rscript -e "install.packages(c('mc2d', 'truncnorm', 'yaml', 'rstudioapi'), repos='https://cloud.r-project.org')"

# Install Python dependencies
RUN pip install --no-cache-dir boto3
RUN pip install --no-cache-dir awslambdaric

# Copy all code and scripts into Lambda task root
COPY . $LAMBDA_TASK_ROOT
RUN mv $LAMBDA_TASK_ROOT/lambda_function/* $LAMBDA_TASK_ROOT

# Set Python path for Lambda
ENV PYTHONPATH=$PYTHONPATH:$LAMBDA_TASK_ROOT

# Default Lambda entrypoint
ENTRYPOINT ["/usr/local/bin/python", "-m", "awslambdaric"]
CMD ["lambda_handler.lambda_handler"]
