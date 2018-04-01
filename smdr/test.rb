#!/usr/bin/ruby
require 'rubygems'
require 'redis'
require 'twitter'
require 'uuid'

Twitter.configure do |config|
  config.consumer_key = 'yROXy0F7Gq2gnNfD5gnvA'
  config.consumer_secret = 'YUiNGsc1XvBdBL6kNc0vnalynozdKjwPuNAnAeLAEsU'
  config.oauth_token = '493794330-j4mN7RpcfGPkQCSn1Eh2qQKhetmGIi1yTjo6yTlR'
  config.oauth_token_secret = 'l5q9yPagYoNLFOHdgMRA0jRsaJ2t8KcZnwahBWM3CA'
end

Twitter.update("I'm tweeting with @gem!")

puts Twitter.user("sferik").location
