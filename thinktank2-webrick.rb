# -*- coding: utf-8 -*-
################################################################################################################
# Webrick     : routing of emacs request
# SimpleTimer : maintenance
################################################################################################################
require 'pp'
require 'webrick'
require 'benchmark'
require 'socket'
require 'json'
require 'uri'
require 'erb'
require "rbconfig"

# require_relative 'simpletimer'

def execute_server ()
  ### ----------------------------------------------------------------------------------------------------------
  ### webrick server 起動＆初期化
  ### ----------------------------------------------------------------------------------------------------------
  memodir  = ThinktankRoot.memodir
  tempdir  = ThinktankRoot.tempdir
  syncdir  = ThinktankRoot.syncdir
  url      = ThinktankRoot.property( "Thinktank.Host.thinktank:url" )  # "http://127.0.0.1:20080/thinktank/

  url.sub!( /20080/, "20090" )
  
  http, host, port, root = url.split( /[\/:]+/ )                       # [ http, 127.0.0.1, 20080, thinktank ]
  root = "/#{root}/"

  server = ThinktankServer.new( { :BindAddress => host, :Port => port, :DocumentRoot => root, :Tempdir => tempdir } )

  ### ----------------------------------------------------------------------------------------------------------
  ### thinktankobject初期化 
  ### ----------------------------------------------------------------------------------------------------------
  thinktank = ThinktankRoot.create_root

  ### ----------------------------------------------------------------------------------------------------------
  ### サーバールーティング
  ### ----------------------------------------------------------------------------------------------------------

  # 
  # メモ用設定：　/thinktank/memos/xxxx-xx-xx-xxxxxx.howm(html)
  #
  server.mount_proc( "#{root}memos" ){|req,res|
    req.extend ThinktankMemoRequest
    req.show_request

    # ここの memo や index を erbfile内で使用する
    case req.action
    when "show"    then memo  = thinktank.get_memo( req.id )
    when "update"  then memo  = thinktank.update_memo!( req.id, req.body )
    when "destroy" then memo  = thinktank.delete_memo!( req.id )
    when "create"  then memo  = thinktank.create_memo!( req.id, req.body )     # ここにはこない createもupdateで処理している 
    when "index"   then index = thinktank.index_object( req.lookup, req.body )
    end
    
    res['Content-Type'] = "text/#{req.fmt};charset=utf-8"
    res['Filename']     = ( memo.filename rescue "no name" )
    res.body = ERB.new( IO.read( req.erbpath ), nil, '-' ).result( binding ) if req.erbpath  # erb変換結果が返る
    memo = index = nil
  }

  #
  # CSS/JS設定：　/thinktank/style/xxxx
  # 
  server.mount_proc( "#{root}style" ){|req,res|
    req.extend ThinktankStyleRequest
    res.body = IO.read( req.stylepath )
    res['Content-Type'] = "text/css;charset=utf-8"
  }

  #
  # HyperEstraier設定：　/thinktank/estseek.cgi
  #
  osn = case RbConfig::CONFIG["target_os"].downcase
        when /mswin(?!ce)|mingw|cygwin|bccwin/ then "win"
        when /linux/ then "linux"
        else "mac"
        end
  server.mount( '/thinktank/estseek.cgi', 
                WEBrick::HTTPServlet::CGIHandler, 
                File.dirname( File.expand_path(__FILE__)) + "/HE/#{osn}/estseek.cgi" )

=begin
  #
  # 写真用設定：　/photo/xx-xx-xx-xxxxxx-xxx.jpg
  #
  server.mount_proc( "#{root}/photo" ){|req, res|

    server.log "[PHOTO] #{req.path}\n"

    if m = req.path.match( /\/photo\/((([0-9]{2})\-[0-9]{2}\-[0-9]{2})\-([0-9]{6})\-([0-9]{3}).jpg)/i ) then
      filename = m[1]
      
      year = ( 80 < m[3].to_i ? "19" : "20" ) + m[3]
      dir  = ( 80 < m[3].to_i ? "19" : "20" ) + m[2]
      filepath = "#{imagedir}#{year}/#{dir}/#{filename}"
      puts "PATH:#{filepath}"
      res.body           = open( filepath ){|file| file.binmode; file.read }
      res.content_length = File.stat( filepath ).size
      res.content_type   = WEBrick::HTTPUtils.mime_type( req.path, WEBrick::HTTPUtils::DefaultMimeTypes )
    end
  }

=end
  
  ### ----------------------------------------------------------------------------------------------------------
  ###
  ### Timer処理 : 
  ### 
  ### ----------------------------------------------------------------------------------------------------------
  
  #SimpleTimer.new(300){ puts ( thinktank.dump ? "" : "no need to be " ) + Time.now.strftime( "updated [%m/%d %H:%M]" ) }

  
  ### ----------------------------------------------------------------------------------------------------------
  ##
  ## webrick起動
  ##
  ### ----------------------------------------------------------------------------------------------------------
  trap( "INT" ){ server.shutdown }
  server.start

end








#---------------------------------------------------------------------------------------------------------------
# ThinktankServer
#---------------------------------------------------------------------------------------------------------------
module WEBrick
  module HTTPServlet
    class ProcHandler < AbstractServlet
      alias do_PUT    do_GET
      alias do_DELETE do_GET
    end
  end
end

class ThinktankServer < WEBrick::HTTPServer
  def initialize ( *param )
    super
    @logfile = "#{config[:Tempdir]}server.log"
    File.delete( @logfile ) rescue ""
    
    puts "WEBRICK>> " + [:BindAddress, :Port, :DocumentRoot, :Tempdir].map{|key| "#{key}:#{config[key]}  " }.join

  end

  def log ( str )
    File.open( @logfile, "a" ){|f|
      str << "\n"
      str.each_line{|lin| f.puts "%s| #{lin}" % Time.now.strftime("%H:%M:%S") }
    }
  end
end



#---------------------------------------------------------------------------------------------------------------
# ThinktankRequest
#---------------------------------------------------------------------------------------------------------------
module ThinktankRequest
  attr_accessor :srcdir

  def self.extended (req)
    req.srcdir = "#{File.expand_path(File.dirname( __FILE__ ))}/"
  end
end


module ThinktankStyleRequest
  include ThinktankRequest
  attr_accessor :styledir, :stylepath

  def self.extended (req)
    super(req)
    req.styledir  = "#{req.srcdir}style/"
    req.stylepath = req.styledir + File.basename( req.path )

    puts "STYLEREQUEST>> style was requested --> #{req.path}"
  end
end


module ThinktankMemoRequest
  include ThinktankRequest
  attr_accessor :ua, :meth, :fmt, :res, :id, :act, :urlopts, :urlhash
  attr_accessor :erbpath
  attr_accessor :lookup, :optional, :debug, :action


  @@url_regexp = Regexp.new( 'memos(\/(\d{4}\-\d{2}\-\d{2}\-\d{6}))?(\/(show|index|new|edit))?(\.(.+))?$' )
  @@ua_regexp  = Regexp.new( '(emacs|chrome|firefox)', Regexp::IGNORECASE )
  
  def self.extended (req)

    super(req)
    url_match = @@url_regexp.match( req.path )

    req.meth    = req.request_method
    req.ua      = ( @@ua_regexp.match( req['User-Agent'] ) ? $1.downcase : "" )
    req.res     = "memos"
    req.id      = url_match[2] || ""
    req.act     = url_match[4] || ""
    req.fmt     = url_match[6] || ""
    req.urlopts = req.query_string
    
    req.urlhash = Hash[ *req.urlopts.split('&').map{|kv| URI.unescape(kv).split('=',2) }.flatten ] rescue {}
    req.lookup   = JSON.parse( req.urlhash['lookup'] ) || {} rescue {}
    req.optional = JSON.parse( req.urlhash['optional'] ) || {} rescue {}
    req.debug    = req.urlhash['debug'] || ""
    req.action   = case [ req.meth, req.act ]
                     # when ["GET", "new"]   then "new"
                     # when ["GET", "edit"]  then "edit"
                     # when ["GET", "show"]  then "show"
                   when ["GET", "index"] then "index"
                   when ["POST", ""]     then "create"     # emacsはupdateのみ使用, 
                   when ["GET", ""]      then "show"
                   when ["PUT", ""]      then "update"
                   when ["DELETE", ""]   then "destroy"
                   else "error"
                   end
    req.erbpath  = [".#{req.ua}",""].map{|x| "#{req.srcdir}memos/#{req.action}.#{req.fmt}#{x}.erb" }.find{|x| File.exists?(x) }

    case req.debug
    when "request"  then req.show_request
    when "lookup"   then puts "MEMOREQUEST>> #{req.lookup}"
    when "optional" then puts "MEMOREQUEST>> #{req.optional}"
    when "urlhash"  then puts "MEMOREQUEST>> #{req.urlhash}"
    else puts "MEMOREQUEST>> memo was requested --> #{req.path}"
    end
    
  end
  def body () URI.unescape( super || "" ) end
  def show_request ()
    puts "MEMOREQUEST>> path --> #{path}"
    puts "MEMOREQUEST>> meth --> #{meth}"
    puts "MEMOREQUEST>> ua --> #{ua}"
    puts "MEMOREQUEST>> res --> #{res}"
    puts "MEMOREQUEST>> id --> #{id}"
    puts "MEMOREQUEST>> act --> #{act}"
    puts "MEMOREQUEST>> fmt --> #{fmt}"
    puts "MEMOREQUEST>> lookup --> #{lookup}"
    puts "MEMOREQUEST>> optional --> #{optional}"
    puts "MEMOREQUEST>> action --> #{action}"
    puts "MEMOREQUEST>> erbpath --> #{erbpath}"
    puts "MEMOREQUEST>> body --> \n#{body[0,100]}\n----------\n"
  end
end





