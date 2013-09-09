#!/bin/sh
set -x
echo "Running user-data script"
apt-get update

cd /home/ubuntu

wget http://download.redis.io/redis-stable.tar.gz
tar xvzf redis-stable.tar.gz
cd redis-stable
make
cp src/redis-server /usr/local/bin/
cp src/redis-cli /usr/local/bin/
redis-cli -h ec2-54-211-155-104.compute-1.amazonaws.com lpush 'workers' `hostname -i`
cd ..

su ubuntu
cd ~

git clone https://github.com/cbare/DOSE.git

wget http://cran.r-project.org/src/contrib/Archive/MASS/MASS_7.3-22.tar.gz

echo "Running config_dose_redis.R"
Rscript DOSE/config_dose_redis.R > /home/ubuntu/config_dose_redis.log 2>&1

echo "Running run_dose_redis.R"
Rscript DOSE/run_dose_redis_worker.R > /home/ubuntu/run_dose_redis_worker_1.log 2>&1 &
Rscript DOSE/run_dose_redis_worker.R > /home/ubuntu/run_dose_redis_worker_2.log 2>&1 &
Rscript DOSE/run_dose_redis_worker.R > /home/ubuntu/run_dose_redis_worker_3.log 2>&1 &
Rscript DOSE/run_dose_redis_worker.R > /home/ubuntu/run_dose_redis_worker_4.log 2>&1 &
