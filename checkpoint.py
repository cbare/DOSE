#!/usr/bin/python
 
# crontab -e
# */20 * * * * /home/ubuntu/checkpoint.py > /home/ubuntu/checkpoint.log 2>&1
 
import os

hosts = []
with open('/usr/local/Rmpi/hostfile.plain') as f:
    for line in f:
        hosts.append(line.split()[0])

os.chdir("/home/ubuntu/checkpoints")

if os.path.exists('checkpoint_counter'):
    with open('checkpoint_counter') as f:
      i = int(f.read())
else:
    i = 1

try:
  os.mkdir("checkpoint%d" % i)
  os.chdir("checkpoint%d" % i)
  for host in hosts:
    os.system('scp -i ~/cbare-sage.pem ubuntu@%s:/home/ubuntu/dose.large.node*.tsv .' % host)
except Exception, e:
  print("Error: " + str(e))
finally:
  os.chdir("/home/ubuntu/checkpoints")
 
with open("checkpoint_counter", "w") as f:
  f.write("%d\n" % (i+1))
