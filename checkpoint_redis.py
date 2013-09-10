import os
import redis

r = redis.Redis()
hosts = r.lrange('workers', 0, 1000)

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
    os.system('scp -o StrictHostKeyChecking=no -o CheckHostIP=no -i ~/cbare-sage.pem ubuntu@%s:/home/ubuntu/dose.large.node*.tsv .' % host)
except Exception, e:
  print("Error: " + str(e))
finally:
  os.chdir("/home/ubuntu/checkpoints")
 
with open("checkpoint_counter", "w") as f:
  f.write("%d\n" % (i+1))
