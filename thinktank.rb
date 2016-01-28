# -*- coding: utf-8 -*-
################################################################################################################
# Thinktankメモ管理
################################################################################################################
require 'minitest/autorun'
require 'minitest/unit'
#require 'test/unit/testcase'
#require 'minitest/test'

if ARGV[0] == "test2" then
  load 'thinktank2-object.rb'
  load 'thinktank2-webrick.rb'

  class TestThinktankRoot < MiniTest::Unit::TestCase
    thinktank = ThinktankRoot.create_root
    index = thinktank.index_object( JSON.parse( "" ) )
    
  end


elsif ARGV[0] == "test1" then
  load 'thinktank2-object.rb'
  load 'thinktank2-webrick.rb'

  class TC_Foo < MiniTest::Unit::TestCase

    def setup
      @thinktank = ThinktankRoot.new
    end

    def test_registd_memo
      memo = ThinktankMemo.new( @thinktank, "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )

      @thinktank << memo
      assert_equal( @thinktank.ThinktankMemo.size, 1 )
      assert_equal( @thinktank.ThinktankMemo[0].id, "0000-00-00-000000" )

      @thinktank >> memo
      assert_nil( @thinktank.ThinktankMemo[0] )
      assert_equal( @thinktank.ThinktankMemo.size, 0 )

    end

    def test_create_memo

      puts @thinktank.list_children

      #memo = ThinktankMemo.create_memo( @thinktank, nil, "* test\n** test2" )

      #@thinktank << memo
      #assert_equal( @thinktank.ThinktankMemo.size, 1 )
      #assert_match( @thinktank.ThinktankMemo[0].content, "test" )

      #@thinktank >> memo
      #assert_nil( @thinktank.ThinktankMemo[0] )
      #assert_equal( @thinktank.ThinktankMemo.size, 0 )
    end
  end





elsif ARGV[0] == "version2.1" then
  ARGV[0] = nil

  load 'thinktank2-object.rb'
  load 'thinktank2-webrick.rb'
  
  puts
  puts "memodir:#{ThinktankRoot.memodir}"
  puts "tempdir:#{ThinktankRoot.tempdir}"
  puts "syncdir:#{ThinktankRoot.syncdir}"
  puts "property:" + ThinktankRoot.property( "Thinktank.Host.thinktank:url" )  #   "http://127.0.0.1:20080/thinktank/

  execute_server

  

=begin

elsif ARGV[0] == "version2" then

  ARGV[0] = nil

  load 'thinktank2-object.rb'
  load 'thinktank2-webrick.rb'
  
  class TC_Foo < Test::Unit::TestCase

    def tst_zenhan
      t = ThinktankObject.new( self )
      
      puts
      puts t._ljust( "出力を左詰めにします幅の指定がなければ 意味がありません。", 31, " " )
      puts t._ljust( "東京と特許許可今日", 31, " " )
      puts "東京と特許許可今日".byteslice(0,10)
      puts "東京と特許許可今日".byteslice(0,11)
      puts t._fmt( "東京と特許許可今日", "%-11s")
      puts t._fmt( "東京と特許許可今日", "%11s")

    end

    def test_index_memo

      thinktank = ThinktankRoot.new
      thinktank << ThinktankMemo.new( thinktank, "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )
      thinktank << ThinktankMemo.new( thinktank, "#{ThinktankRoot.memodir}0000-00-00-000001/0000-00-00-000001.howm" )
      thinktank << ThinktankMemo.new( thinktank, "#{ThinktankRoot.memodir}0000-00-00-000002/0000-00-00-000002.howm" )
      thinktank.index_memo( { "query" => [ "[ThinktankTodoTag]", "memo" ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankMarkup]", "memo" ] } )
      thinktank.index_memo( { "query" => [ "id==0000-00-00-000002", "ThinktankChapter" ] } )
      thinktank.index_memo( { "query" => [ "title==Pending" ] } )
      thinktank.index_memo( { "query" => [ "title=~(Pending|title)" ] } )
      thinktank.index_memo( { "query" => [ "title>>press" ] } )
      thinktank.index_memo( { "query" => [ 'title<<StoragePCServer' ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankTodoTag]", "ega:=current" ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankTodoTag]", "name==INBOX", "memo" ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankTodoTag]", "ega:=current", "current:=ega - ega" ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankTodoTag]", "ega:=current", "current:=ega & null" ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankTodoTag]", "ega:=current", "current:=ega + null" ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankTodoTag]", "ega:=current", "current:=ega | null" ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankTodoTag]", "name==INBOX", "ega:=current", "current:=null", "memo" ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankChapter]", "content=~orexin" ] } )
      thinktank.index_memo( { "query" => [ "[ThinktankChapter]", "content=~orexin", "content=~NPY" ] } )

      puts "\n", thinktank.index_memo( { "query" => [ "[ThinktankMemo]" ], "sort" => "id", "order" => "dsc", "list" => "(id).howm | (title,%40s)" } )
      puts thinktank.index_memo( { "query" => [ "[ThinktankMemo]" ], "sort" => "id", "order" => "dsc", "first" => "(id)" } )
      puts thinktank.index_memo( { "query" => [ "[ThinktankMemo]" ], "sort" => "id", "order" => "asc", "first" => "(id)" } )

      thinktank = ThinktankRoot.create_root()
      #puts "\n", tt.index_memo( { "query" => [ "[ThinktankMemo]", "title=~Orexin" ], "sort" => "id", "order" => "dsc", "list" => "(id).howm | (title,%40s)" } )
      #puts "\n", tt.index_memo( { "query" => [ "[ThinktankTime]", "ymd>=2014-01-01", "ymd<=2014-02-01", "memo", "ThinktankChapter", "ThinktankTodoTag" ], "list" => "(memo.id) | (name)" } )
      #puts "\n", tt.index_memo( { "query" => [ "[ThinktankTime]", "ymd>=2014-01-01", "ymd<=2014-02-01", "memo", "ThinktankChapter", "ThinktankTodoTag" ], "count" => "name", "export" => "csv" } )
      #puts "\n", tt.index_memo( { "query" => [ "[ThinktankTime]", "ymd>=2014-01-01", "ymd<=2014-02-01" ], "count" => "name", "export" => "csv" } )

      #puts "\n", tt.index_memo( { "query" => [ "[ThinktankMemo]" ], "crosscount" => "mon,weekday", "export" => "emacs" } )
      #puts "\n", tt.index_memo( { "query" => [ "[ThinktankTitleTag]" ], "count" => "name", "order" => "dsc", "limit" => "30", "export" => "emacs" } )
      puts "TEST\n"
      puts thinktank.index_object( { "query" => [ "[ThinktankMemo]" ], "limit" => "30", "list" => "(id).howm | (title,%40s)" } )
      
    end

    def tst_verup
      thinktank = ThinktankRoot.new
      thinktank.create_memo!( "0000-00-00-000006", "test" )
      thinktank.verup_memo!( "0000-00-00-000006", "test test test", :rev )
      thinktank.verup_memo!( "0000-00-00-000006", "test test 2345", :rev )

    end

    def tst_update
      thinktank = ThinktankRoot.new
      thinktank.create_memo!( "0000-00-00-000006", "test" )
      thinktank.update_memo!( "0000-00-00-000006", "test\nできた" )
      thinktank.update_memo!( "0000-00-00-000006", "test\nできた\n東京都" )
      # thinktank.update_memo!( "0000-00-00-000006", "test\n東京都" )
    end

    def tst_create
      thinktank = ThinktankRoot.new
      thinktank << ThinktankMemo.new( thinktank, "#{ThinktankRoot.memodir}0000-00-00-000002/0000-00-00-000002.howm" )
      # thinktank << ThinktankMemo.new( thinktank, "#{ThinktankRoot.memodir}0000-00-00-000006/0000-00-00-000006.howm" )
      # thinktank << ThinktankMemo.create_memo!( thinktank, "0000-00-00-000006", "test" )
      thinktank.create_memo!( "0000-00-00-000006", "test" )
      assert_equal( thinktank.ThinktankMemo["0000-00-00-000006"].id, "0000-00-00-000006" )
      thinktank.delete_memo!( "0000-00-00-000006" )
      assert_nil( thinktank.ThinktankMemo["0000-00-00-000006"] )
      # pp thinktank.ThinktankMemo[ "0000-00-00-000005" ].title


    end

    def test_dump
      thinktank = ThinktankRoot.create_root
      puts thinktank.ThinktankMemo.size
 
      #thinktank = ThinktankRoot.create_root
      #assert_equal( thinktank.ThinktankMemo[0].ThinktankChapter[0], thinktank.ThinktankChapter[0] )
      
      # puts thinktank.ThinktankMemo[0].id
      # thinktank.ThinktankMemo("0000-00-00-000000").ThinktankChapter.each{|c| puts c.address }
      puts thinktank.ThinktankMemo["0000-00-00-000000"].class
      assert_equal( thinktank.ThinktankMemo[0].id, thinktank.ThinktankMemo["0000-00-00-000000"].id )
    end


    def test_ttchapterax

      thinktank = ThinktankRoot.new()
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000001/0000-00-00-000001.howm" )
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000002/0000-00-00-000002.howm" )
      assert_equal( thinktank.ThinktankMemo[0].ThinktankChapter[0], thinktank.ThinktankChapter[0] )
      assert_not_nil( thinktank.ThinktankChapter[0].title.index( "HOHM" ) )
      assert_equal( thinktank.ThinktankMemo[0].ThinktankChapter[0].title, thinktank.ThinktankMemo[0].title )
      # assert_equal( thinktank.ThinktankMemo[0].ThinktankChapter[4], thinktank.ThinktankMemo[0].ThinktankChapter[21].prevSiblingChapter )
      # assert_equal( thinktank.ThinktankMemo[0].ThinktankChapter[4].nextSiblingChapter, thinktank.ThinktankMemo[0].ThinktankChapter[21] )

      # thinktank.ThinktankMemo[0].ThinktankChapter.each{|ttc| puts "[#{ttc.number}] [#{ttc.heading}]" }

      # thinktank.ThinktankProperty.each{|item| puts "#{item.key}:#{item.value}" }

      # puts thinktank.ThinktankMemo[0].to_display

      # thinktank.ThinktankChapter.each{|chap| puts "#{chap.address}" }
      # thinktank.ThinktankTitleTag.each{|tttt| puts tttt.name }
      # thinktank.ThinktankTag.each{|tttag| puts "#{tttag.class}::[#{tttag.name}]" }

      # thinktank.ThinktankChapterCreatedTime.each{|ttcct| puts "#{ttcct.chapter.title}" }
      # thinktank.ThinktankStateChangedTime.each{|ttsct| puts "#{ttsct.chapter.title} from #{ttsct.pre_state} to #{ttsct.post_state}" }

    end

         

    def test_ttmemo
      thinktank = ThinktankRoot.new()
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000001/0000-00-00-000001.howm" )
      
      assert_equal( thinktank.ThinktankMemo[0].class, ThinktankMemo )
      assert_equal( thinktank.ThinktankMemo[0].id, "0000-00-00-000000" )
      assert_equal( thinktank.ThinktankMemo[0].filename, "0000-00-00-000000.howm" )
      assert_not_nil( thinktank.ThinktankMemo[0].dirname.index(  thinktank.memodir ) )
      assert_not_nil( thinktank.ThinktankMemo[0].content.match( "Dock2 GTPase screening" ) ) 
      
      # memoを一旦削除、再登録して元に戻ること確認
      ttobj1 = thinktank.ThinktankObject.group_by{|obj| obj.class }.map{|key,val| "#{key}:#{val.length} " }.sort.join

      thinktank.delete_memo( "0000-00-00-000000" )
      
      assert_equal( thinktank.ThinktankMemo.find{|m| m.id == "0000-00-00-000000" }, nil )
      ttobj2 = thinktank.ThinktankObject.group_by{|obj| obj.class }.map{|key,val| "#{key}:#{val.length} " }.sort.join
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )
      assert_not_equal( ttobj1, ttobj2 )

      ttobj3 = thinktank.ThinktankObject.group_by{|obj| obj.class }.map{|key,val| "#{key}:#{val.length} " }.sort.join
      assert_not_equal( thinktank.ThinktankMemo.find{|m| m.id == "0000-00-00-000000" }, nil )
      assert_equal( ttobj1, ttobj3 )
      
    end



    def test_ttroot_tttime
      thinktank = ThinktankRoot.new()
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000001/0000-00-00-000001.howm" )
      assert_not_nil( thinktank.to_s.index( Time.now.strftime( "%Y%m%d%H%M" ) ) )

      assert_equal( thinktank.ThinktankMemo[0].year, "0000" ) 
      assert_equal( thinktank.ThinktankMemo[0].sec, "00" )
      assert_equal( thinktank.ThinktankMemo[0].wday, nil )
      assert_equal( thinktank.ThinktankMemo[0].ymd, nil )
      assert_equal( thinktank.ThinktankMemo[0].start.year, "0000" )
      assert_equal( thinktank.ThinktankMemo[0].due.year, "0000" )
      assert_equal( thinktank.ThinktankMemo[0].orgtime, nil )

      assert_equal( thinktank.ThinktankMemo[0].to_i, 0 )
      assert_equal( thinktank.ThinktankMemo[0].to_s, "00000000000000" )
      assert_equal( thinktank.ThinktankMemo[0] <=> thinktank.ThinktankMemo[1], -1 )
      assert_equal( thinktank.ThinktankMemo[0] <=> thinktank, -1 )
    end

    def test_ttroot_ttparent
      thinktank = ThinktankRoot.new()
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )
      assert_equal( thinktank.ThinktankMemo.size, 1 )
    end

    def test_ttroot
      thinktank = ThinktankRoot.new()
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )

      assert_equal( thinktank.class, ThinktankRoot )
      assert_equal( thinktank.ThinktankMemo[0].root, thinktank )
      assert_equal( thinktank.ThinktankMemo[0].root.class, ThinktankRoot )

    end


    def test_ttmemo_ttfile
      thinktank = ThinktankRoot.new()
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )

      assert_not_nil( thinktank.ThinktankMemo[0].filepath.index( thinktank.memodir ) )
    end

    def test_ttmemo_ttdiff
      thinktank = ThinktankRoot.new()
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )

      assert_not_nil( thinktank.ThinktankDiff[0].filename.index( thinktank.ThinktankMemo[0].id ) )
      assert_equal( thinktank.ThinktankDiff[0].filename, thinktank.ThinktankMemo[0].ThinktankDiff[0].filename )
    end

    def test_ttmemo_ttversion
      thinktank = ThinktankRoot.new()
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )

   by   assert_not_nil( thinktank.ThinktankVersion[0].filename.index( thinktank.ThinktankMemo[0].id ) )
      assert_equal( thinktank.ThinktankVersion[0].filename, thinktank.ThinktankMemo[0].ThinktankVersion[0].filename )
    end

    def test_ttmemo_ttbundled
      thinktank = ThinktankRoot.new()
      thinktank << ThinktankMemo.new( thinktank , "#{ThinktankRoot.memodir}0000-00-00-000000/0000-00-00-000000.howm" )

      assert_equal( thinktank.ThinktankBundled[0].filename, thinktank.ThinktankMemo[0].ThinktankBundled[0].filename )
    end


    def setup
      #@thinktank = ThinktankRoot.new
    end

  end




  # execute_server
  

else
  load 'thinktank-util.rb'
  load 'thinktank-object.rb'
  load 'thinktank-main.rb'
  load 'thinktank-memo.rb'
  load 'thinktank-memo-associated.rb'
  load 'thinktank-node.rb'
  load 'thinktank-node-associated.rb'
  load 'thinktank-webrick.rb'

  case ARGV[0]
  when "test5"
    memo_dir = get_memodir
    tt = Thinktank.new( memo_dir )
    Dir::glob("#{memo_dir}????-??-??-??????/0000-00-00-00000?.howm").each{|filename| tt << Memo.new( tt , filename ) }
    
    #puts ">> #{ tt.memo("0000-00-00-000001").title }"
    puts "-------------------------------------------------------------------"
  when "test4"
    memo_dir = get_memodir
    tt = Thinktank.new( memo_dir )
    Dir::glob("#{memo_dir}2007/02/2007-02-07-113212/2007-02-07-113212.howm").each{|filename| tt << Memo.new( tt , filename ) }

    # puts ">> #{ tt.memo("2007-02-07-113212").title }"
    puts ">> #{ tt.memo("2007-02-07-113212").node[0].linetimetag[4].point }"
    puts "-------------------------------------------------------------------"

  when "test3"
    memo_dir = get_memodir
    tt = Thinktank.new( memo_dir )
    Dir::glob("#{memo_dir}????-??-??-??????/0000-00-00-00000?.howm").each{|filename| tt << Memo.new( tt , filename ) }


    #puts ">> #{ tt.memo("0000-00-00-000001").title }"
    #puts ">> #{ tt.memo("0000-00-00-000001").node.collect{|n| n.linetimetag.collect{|l| l.title } } }"
    puts ">> " + tt.system_property( "Thinktank.Host.thinktank.route" )
    puts "-------------------------------------------------------------------"

  when "test2"
    config_fil = File.dirname(__FILE__) + "/memodir@" + Socket.gethostname.upcase + ".conf"
    memo_dir   = File.open( config_fil, "r" ).gets.strip rescue ""

    tt = Thinktank.new( memo_dir )
    Dir::glob("#{memo_dir}????-??-??-??????/0000-00-00-00000?.howm").each{|filename| tt << Memo.new( tt , filename ) }
    puts tt.system_property "Thinktank.Host.thinktank.port2"
    tt.system_property "Thinktank.Host.thinktank.port2", "10"
    puts tt.system_property "Thinktank.Host.thinktank.port2"
    puts "-------------------------------------------------------------------"

  when "test"
    $:.unshift File.dirname(__FILE__)
    $:.unshift ( File.dirname(__FILE__) + '/icalendar' )
    require 'icalendar.rb'

    
    thinktank = tmp = nil

    # thinktank = Thinktank.load("E:/DropBox/MyData/tt/", "E:/tmp/" )
    thinktank = Thinktank.load("e:/DropBox/MyData/tt/", "e:/tmp/", nil )
    # thinktank = Thinktank.load("/Users/gogowooky/DropBox/MyData/tt/", "/Users/gogowooky/tmp/", nil )

    #num = 14
    contents = thinktank.memo("0000-00-00-000001").contents
    #thinktank.memo("0000-00-00-000001").update_with( contents )
    #thinktank.dump
    puts "=================================================="
    puts thinktank.memo("0000-00-00-000001").get_property( ["Thinktank", "Host", "thinktank", "host"] )
    puts thinktank.memo("0000-00-00-000001").get_property( ["Thinktank", "Host", "thinktank", "syncdir"] )
    puts "=================================================="
    #puts "================================================="
    #puts thinktank.memo[num].node[1].contents
    #puts "================================================="
    

    # thinktank.dump
    # thinktank = Thinktank.load("/Users/gogowooky/Dropbox/MyData/tt/", "/Users/gogowooky/tmp/")
    # thinktank.reload

    # when File.exists? ARGV[0]
    
    # ARGV[0] をもとに memodir を設定する

  else

    execute_server

  end
=end      


end



