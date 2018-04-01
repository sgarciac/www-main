#!/usr/bin/ruby
require 'rubygems'
require 'redis'
require 'cgi'
require 'uuid'



r = Redis.new
cgi = CGI.new
h = cgi.params 
ip = cgi.remote_addr
last = (r.exists "ip:"+ip) ? (r.get "ip:"+ip) : 0


if ((Time.now.to_i - last.to_i) < 120)
	puts cgi.header
	secondsleft = (120 - (Time.now.to_i - last.to_i))
	puts "<html><body>Sorry! You already sent a secret message recently. You can send a new message in "+secondsleft.to_s+ " seconds</body></html>"
	exit
end

user = cgi['user'].gsub(/[^[:alnum:]_]/, '')[0,15] 
msg = cgi['msg'][0,2000]

msgfile = "msgs/"+UUID.new.generate+".txt"
File.open(msgfile, 'w') do |f2|  
   f2.puts "@"+user+":"+"\n\n"
   f2.puts msg+"\n\n"
   f2.puts "http://www.crazyrobot.net/smdr"
end 


r.lpush 'smdr.requests', '@'+user+" : you've got mail at: http://www.crazyrobot.net/smdr/"+msgfile
r.set "ip:"+ip, Time.now.to_i

puts cgi.header
puts "<html><body>Ok! We currently have " + (r.llen 'smdr.requests').to_s+ " messages to process."
puts "</body></html>"
