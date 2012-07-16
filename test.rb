require 'sinatra'

get '/' do
  params.inspect
end

post '/' do
  params.inspect
end
