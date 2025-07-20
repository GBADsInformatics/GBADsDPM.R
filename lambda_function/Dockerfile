FROM python:3.13-slim

ENV LAMBDA_TASK_ROOT /var/task
WORKDIR ${LAMBDA_TASK_ROOT}

# Add CRAN repository for R packages
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
        libxt6 \
        libpng-dev \
        libjpeg-dev \
        libcairo2-dev \
        libbz2-dev \
        libzstd-dev \
        liblzma-dev \
        wget \
        git && \
    rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN Rscript -e "install.packages(c('mc2d', 'truncnorm', 'yaml', 'rstudioapi'), repos='https://cloud.r-project.org')"

# Copy the entire lambda_function directory into /var/task
COPY ./lambda_function/requirements.txt /tmp/requirements.txt
RUN pip install -r /tmp/requirements.txt
COPY . ${LAMBDA_TASK_ROOT}

# Add AWS Lambda Runtime Interface Emulator for local testing
ADD https://github.com/aws/aws-lambda-runtime-interface-emulator/releases/latest/download/aws-lambda-rie /usr/bin/aws-lambda-rie
RUN chmod +x /usr/bin/aws-lambda-rie

ENV PYTHONPATH=PYTHONPATH:${LAMBDA_TASK_ROOT}

# Set the default command for Lambda (Python handler)
CMD ["/usr/bin/aws-lambda-rie", "python3", "-m", "awslambdaric", "lambda_function.lambda_handler.lambda_handler"]
