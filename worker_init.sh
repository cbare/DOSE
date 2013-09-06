#!/bin/sh
echo "Running user-data script"
apt-get update

wget http://download.redis.io/redis-stable.tar.gz
tar xvzf redis-stable.tar.gz
cd redis-stable
make
cp src/redis-server /usr/local/bin/
cp src/redis-cli /usr/local/bin/
redis-cli -h ec2-54-211-155-104.compute-1.amazonaws.com lpush 'workers' `hostname -i`

git clone https://github.com/cbare/DOSE.git

wget http://cran.r-project.org/src/contrib/Archive/MASS/MASS_7.3-22.tar.gz

echo "Running config_dose_redis.R"
Rscript DOSE/config_dose_redis.R 2>&1 config_dose_redis.log

echo "Running run_dose_redis.R"
Rscript DOSE/run_dose_redis.R 2>&1 run_dose_redis.log &
