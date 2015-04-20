# -*- coding: utf-8 -*-
################################################################################################################
# thinktank-util.rb
################################################################################################################
require 'forwardable'
require 'socket'

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 基本クラス
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class Thinktank < Object
end

# ..............................................................................................................
class ThinktankObject < Thinktank
  attr_reader :parent, :root, :ttid
  @@ttid = 1

  def initialize ( parent )
    @parent, @ttid = parent, @@ttid
    @@ttid = @@ttid + 1
    @status = {}
  end
  def root () p = self; ( p = p.parent ) while p.parent; p end
  def === ( obj ) @ttid === obj.ttid end  

  @@display_format = ""   # ex) "(id).howm | (ThinktankDiff[0].ymd,%-30s) | (header)"

  def self.display_format= ( fmt ) @@display_format = fmt end
  def to_display () @@display_format.gsub( /\(([^,\(\)]+)(?:,([^,\(\)]+))?\)/ ){|m| _fmt( eval( "self.#$1" ), $2 ) } end
  def clsid () self.class.to_s end

  def _fmt( str, fmt ) #  文字列表示では全角:半角=2:1で幅合わせする
    if /%(\-)?([0-9]+)?s/.match( fmt ) then
      fmt.sub( $&, $1 ? _ljust( str, $2.to_i ) : _rjust( str, $2.to_i ) )
    else
      fmt ? "#{fmt}" % str : str
    end
  end

  def _get_width ( str ) str.each_char.map{|c| /[ -~｡-ﾟ]/ =~ c ? 1 : 2 }.reduce( 0, &:+ ) end
  def _trim( str, size ) str.slice!(-1,1) while size <= _get_width( str ); str end
  def _ljust( str, size, pad = " " ) str = _trim( str, size ); str + pad * ( size - _get_width(str) ) end
  def _rjust( str, size, pad = " " ) str = _trim( str, size ); pad * ( size - _get_width(str) ) + str end
end

# ..............................................................................................................
module ChildOfMemo
  attr_reader :size, :offset

  def memo ()
    case
    when self.class               == ThinktankMemo then self
    when self.parent.class        == ThinktankMemo then self.parent
    when self.parent.parent.class == ThinktankMemo then self.parent.parent
    end
  end
  
end

# ..............................................................................................................
class ThinktankTime < ThinktankObject
  include Comparable
  attr_accessor :time
  attr_reader :start, :due
  attr_reader :year, :month, :day, :hour, :min, :sec, :wday
  attr_reader :to_s, :to_i, :to_id
  attr_accessor :period

  attr_reader :weekday, :ymd, :ym, :hm, :org, :orgtime
  attr_reader :mon, :ymon, :hm15, :hm30
  
  def initialize ( parent, tim, tim2 = nil )
    super( parent )
    @period = tim2
    @start, @due = ( tim2 ? [ self, ThinktankTime.new( parent, tim2 ) ].sort : [ self, self ] )
    self.time = tim
  end

  def time= ( tim )
    @ttt = case 
           when tim.is_a?( Time )
             tim.to_a[0,6].map{|tval| '%02d' % tval }.reverse
           when tim.is_a?( String )
             case
             when m = /(\d\d\d\d)\-(\d\d)\-(\d\d)\-(\d\d)(\d\d)(\d\d)/.match(tim) then [ m[1], m[2], m[3], m[4], m[5], m[6] ]
             when m = /(\d\d\d\d)\-(\d\d)\-(\d\d).{0,5}?(\d\d):(\d\d)/.match(tim) then [ m[1], m[2], m[3], m[4], m[5],  nil ]
             when m = /(\d\d\d\d)\-(\d\d)\-(\d\d)/.match(tim)                     then [ m[1], m[2], m[3],  nil,  nil,  nil ]
             when m = /(\d\d\d\d)\/(\d\d)\/(\d\d)/.match(tim)                     then [ m[1], m[2], m[3],  nil,  nil,  nil ]
             else 
               tim = tim.gsub( /[ 　\t]/, '' ).tr('０-９', '0-9') 
               case 
               when m = /(\d\d\d\d)年(\d\d?)月(\d\d?)日/.match(tim) then [ m[1], "0#{m[2]}"[-2,-1], m[3], nil, nil, nil ]
               else [nil,nil,nil,nil,nil,nil]
               end
             end
           else [nil,nil,nil,nil,nil,nil]
           end
    @year, @month, @day, @hour, @min, @sec = @ttt
    @to_s  = "%04d%02d%02d%02d%02d%02d" % @ttt.map(&:to_i)
    @to_id = "%04d-%02d-%02d-%02d%02d%02d" % @ttt.map(&:to_i)
    @to_i = @to_s.to_i  # 比較/sort用, 引き算して使おうとするとおかしいよ(単位がsecではないからだね)
    @time = Time.new( *@ttt ) rescue nil
    @wday = @time.wday rescue nil
    @time
  end
  
  def <=> ( b )
    case
    when ( b.due.to_i <=> @start.to_i ) == -1 then 1
    when ( @due.to_i <=> b.start.to_i ) == -1 then -1
    else 0
    end
  end

  def mon () @wday && ThinktankTime.to_mon( @month ) end
  def ymon () @wday && "#{@year} #{@mon}" end

  def weekday () ThinktankTime.to_weekday( @wday || 7 ) end
  def ymd () @wday && "#{@year}-#{@month}-#{@day}" end
  def ym () @wday && "#{@year}-#{@month}" end
  def hm () @wday && ( @hour ? "#{@hour}:#{@min}" : "--:--" ) end
  def hm15 () @wday && ( @hour ? "#{@hour}:#{'%2d' % ((@min.to_f/15).ceil*15)}" : "--:--" ) end
  def hm30 () @wday && ( @hour ? "#{@hour}:#{'%2d' % ((@min.to_f/30).ceil*30)}" : "--:--" ) end
  def org () @wday && "#{ymd} #{weekday}" end
  def orgtime () @wday && "#{org} #{hm}" end

  @@mons     = [ 'Jan', 'Feb', 'Mar', 'Apl', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', '---' ]
  @@weekdays = [ 'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', '---' ]

  def self.to_mon ( month )    @@mons[ month.to_i - 1 ] end
  def self.to_weekday ( wday ) @@weekdays[ wday ] end
  def self.mons () @@mons[0,12] end
  def self.weekdays () @@weekdays[0,7] end
  def self.cmp_mon( a, b )     @@mons.index(a)     <=> @@mons.index(b) end
  def self.cmp_weekday( a, b ) @@weekdays.index(a) <=> @@weekdays.index(b) end

end


# ..............................................................................................................
class ThinktankChapterArray < Array
  def [] ( address, wid = nil )
    case 
    when address.class == String
      self.find{|item| address == item.address } rescue nil

    when address.class == Fixnum && wid.class == Fixnum
      super( address, wid )
    else
      super( address ) rescue nil
    end
  end
end

class ThinktankMemoArray < Array
  
  def [] ( id, wid = nil )
    case 
    when id.class == String
      self.find{|item| id == item.id } rescue nil

    when id.class == Fixnum && wid.class == Fixnum
      super( id, wid )
    else
      super( id ) rescue nil
    end
  end
end


# ..............................................................................................................
module ThinktankParent
  attr_reader :children

  def initialize ( *args )
    @children = Hash.new()
    super
  end

  def clear () @children.clear end

  def >> ( ttobj ) # { class => Array,,, } に対応
    ancestors = ttobj.class.ancestors.slice_before{|anscestor| anscestor == Thinktank }.to_a[0]
    ancestors.each{|ancestor|
      if @children.has_key?( ancestor.to_s ) then
        @children[ ancestor.to_s ].reject!{|x| x === ttobj }
      end
    }
  end

  def << ( ttobj ) # { class => Array,,, } に対応
    ancestors = ttobj.class.ancestors.slice_before{|anscestor| anscestor == Thinktank }.to_a[0]
    ancestors.each{|ancestor|
      if @children.has_key?( ancestor.to_s ) then
        @children[ ancestor.to_s ] << ttobj
      else
        @children[ ancestor.to_s ] = [ ttobj ]
      end
    }
  end

  def method_missing ( name, *args )
    if name.to_s == "All" then
      @children.values.flatten(1).uniq
    else
      items = @children[ name.to_s ]
      case 
      when name.to_s == "ThinktankMemo"    then ThinktankMemoArray.new( items )
      when name.to_s == "ThinktankChapter" then ThinktankChapterArray.new( items )
      else items
      end
    end
  end

end


# ..............................................................................................................
class ThinktankRoot < ThinktankTime
  include ThinktankParent
  attr_reader :memodir, :tempdir

  def self.hostname () Socket.gethostname.upcase end
  def self.memodir () # memodir@MACHINE.conf から memodirを得る。
    conffile = "#{File.dirname(__FILE__)}/configuration/memodir@#{ThinktankRoot.hostname}.conf"
    begin
      File.open( conffile ){|f| f.gets.strip }
    rescue
      File.open( conffile, "w+" ){|f| f.puts("") }
      puts "confirm memo directry configuration in #{conffile}."
      ""
    end
  end

  @@property_or_markup = Regexp.new '^(.+?)(:|#)(\w+)$'
  def self.property ( address )  
    return unless @@property_or_markup.match( address ) 
    adr, mark, tag = [ $1, $2, $3 ]

    hostname = '@' + ThinktankRoot.hostname
    tt = ThinktankRoot.new()
    Dir::glob( "#{ThinktankRoot.memodir}????-??-??-??????/0000-00-00-00000?.howm" ).each{|filename| tt << ThinktankMemo.new( tt , filename ) }

    chaps = tt.ThinktankChapter.select{|chap| chap.address.sub( hostname , '' ) == adr }
    case mark
    when ':' then chaps.map{|chap| chap.ThinktankProperty }.flatten(1).select{|item| item.key == tag }[-1].value
    when '#' then chaps.map{|chap| chap.ThinktankMarkup   }.flatten(1).select{|item| item.key == tag }[-1].value
    end
  end

  def self.tempdir () ThinktankRoot.property( "Thinktank.Host.thinktank:tempdir" ) end
  def self.syncdir () ThinktankRoot.property( "Thinktank.Host.thinktank:syncdir" ) end
  def self.dumpfilepath () "#{ThinktankRoot.tempdir}thinktank2@#{ThinktankRoot.hostname}.dump" end

  def load_all_memo ()
    self.clear
    puts "THINKTANK-ROOT>> all memo was cleared and reloading."
    Dir::glob( "#{ThinktankRoot.memodir}**/????-??-??-??????/????-??-??-??????.howm" ).each{|fpath|
      fname = File.basename( fpath )
      print "THINKTANK-ROOT>> #{fname}"
      self << ThinktankMemo.new( self , fpath )
      print "\b" * 100
    }
    self.dump
    puts "THINKTANK-ROOT>> all memo was reupdated."
  end

  def load_updated_memo ()
    puts "THINKTANK-ROOT>> updated memo was loading."
    loaded_memos = self.ThinktankMemo.dup
    Dir::glob( "#{ThinktankRoot.memodir}**/????-??-??-??????/????-??-??-??????.howm" ).each{|fpath|
      fname = File.basename( fpath )
      print "THINKTANK-ROOT>> #{fname}"
      if loaded_memo = loaded_memos.find{|m| m.filename == fname } then
        loaded_memos.reject!{|m| m === loaded_memo }
        if ( loaded_memo.updated.time <=> File.mtime( fpath ) ) < 0 then
          puts " was updated. #{loaded_memo.updated.time} -> #{File.mtime( fpath )}"
          self >> loaded_memo
          self << ThinktankMemo.new( self, fpath )
        end
      else
        puts " was newly registered."
        self << ThinktankMemo.new( self, fpath )
      end
      print "\b" * 100
    }
    loaded_memos.each{|m|
      puts "THINKTANK-ROOT>> #{m.filename} was deleted."
      deleted_memo = self.ThinktankMemo.find{|ttm| ttm.id == m.id }
      self >> deleted_memo
    }
    self.dump
    puts "THINKTANK-ROOT>> all updated memo was reloaded."
  end

  def initialize () super( nil, Time.now ) end
  def dump()
    self.time = self.ThinktankFile.max.time + 60
    File.open( ThinktankRoot.dumpfilepath, 'w+b' ){|f| f.write( Marshal.dump( self ) ) }

    puts "THINKTANK-ROOT>> DUMP #{ThinktankRoot.dumpfilepath}"

  end

  def memodir () ThinktankRoot.memodir end
  def tempdir () ThinktankRoot.tempdir end
  def dumped () self end

  def << ( obj )
    # rootの<<でMemoとChapterの子供管理もサポート。(MemoとChapterの<<ではしない)
    # ThinktankMemoとThinktankChapterは子供に自分の名前をつけて、まずはThinktankRootの子供に登録する。　で、Rootがその親の子供として登録してあげる。
    return unless obj
    super( obj )
    obj.parent << obj unless obj.parent == self rescue puts "#{obj.class} "
  end

  def >> ( memo ) # memoの子孫objectもエントリーから外す
    memo.ThinktankObject.each{|child|
      if child.is_a?( ThinktankChapter ) then child.ThinktankObject.each{|gchild| child >> gchild } rescue nil end
      memo >> child
    } rescue nil
    super( memo )
  end
  
  def create_memo! ( id, content )
    self << ( memo = ThinktankMemo.create_memo( self, id, content ) )
    memo
  end
  
  def delete_memo! ( id )
    memo = self.ThinktankMemo[ id ]
    memodir = memo.dirname[0..-2]
    deldir  = memo.dirname[0..-2] + Time.now.strftime( "_deleted_%Y-%m-%d" )
    File.rename( memodir, deldir )
    memo.clear
    self >> memo
  end
  
  def get_memo ( id ) self.ThinktankMemo[ id ] end
  
  def update_memo! ( id, content )
    memo = self.ThinktankMemo[ id ]
    return self.create_memo!( nil, content ) unless memo # id無い場合createに飛ばしている。 
    
    name     = "#{memo.dirname}#{memo.id}_#{Time.now.strftime('%Y-%m-%d')}"
    snappath = name + ".snap"
    diffpath = name + ".diff"
    filepath = memo.filepath
    
    newlines = content.lines
    oldlines = memo.content.lines
    
    added_lines   = newlines - oldlines 
    deleted_lines = oldlines - newlines
    
    if 0 < added_lines.size or 0 < deleted_lines.size then
      File.open( diffpath, 'wb' ){|f|
        added_lines.each{|l|   f.puts "[+] #{l}" }
        deleted_lines.each{|l| f.puts "[-] #{l}" }
      }
      path = memo.filepath
      self >> memo
      if File.exist?( snappath )
        File.delete( filepath ) 
      else
        File.rename( filepath, snappath ) 
      end
      memo = self.create_memo!( id, content )
      self << memo
      print "THINKTANK-ROOT>> #{memo.filename} was updated."
    else
      File.delete( snappath ) if File.exist?( snappath )
      File.delete( diffpath ) if File.exist?( diffpath )
    end

    memo
  end
  
  def verup_memo!( id, content, type )
    memo = self.ThinktankMemo[ id ]
    return nil unless memo
    
    verfile = Dir.glob( "#{memo.dirname}#{memo.id}_????-??-??_??-???-?.ver" ).sort[-1] rescue nil
    ver = if verfile then
            vernum = verfile.split(/[_\-\.]/)[-4,3].join.to_i
            case type
            when :rev   then vernum += 1
            when :minor then vernum = ( vernum + 10 ).round(-1)
            when :major then vernum = ( vernum + 10000 ).round(-4)
            end
            [ vernum/10000, vernum%10000/10, vernum%10000%10 ]
          else
            [ 0, 1, 0 ]
          end

    filepath = memo.filepath
    verfile = "#{memo.dirname}#{memo.id}_" + Time.now.strftime("%Y-%m-%d_") + "%02d-%03d-%01d.ver" % ver
    vertag  = "#+version: %02d.%03d.%01d" % ver
    header,body = content.split("\n",2)
    header.sub!( /ver.[0-9\.]+/, "ver.%02d.%03d.%01d" % ver )
    content = if body && body.sub!( /^#\+version: .*$/, vertag ) then 
                "#{header}\n#{body}"
              else
                "#{header}\n#{vertag}\n#{body}"
              end
    
    File.open( verfile, 'wb' ){|f| f.write( content ) }
    memo.write_content! content
    self >> memo
    self << ThinktankMemo.new( self, filepath )

  end
  
  def index_object ( lookup, senddata = "" )
    
    if lookup.empty? then
      case senddata.strip
      when "synchronize-memo" then load_updated_memo
      when "initialize-memo"  then load_all_memo
      end
      return nil 
    end
    
    # query内変数を確保
    vars = { "current"=>self.All.dup, "all"=>self.All.dup, "null"=>[] }
    print "QUERY>> "
    
    # query実行
    lookup["query"].each{|q|
      print "(#{q}) -> "
      
      if /^\s*:(\w+)\s*(<<|>>|>>>)\s*(.*?)\s*$/.match( q ) then # -------- 変数操作
        var, operator, operand = $1, $2, $3
        vars[var] = case operator
                    when '<<' #
                      case 
                      when operand =~ /:(\w+)/    then ( vars[var] ? vars[var] + vars[$1] : vars[$1] ).uniq.dup
                      when operand =~ /\[(\w+)\]/ then ( vars[var] ? vars[var] + eval( "self.#$1" ) : eval( "self.#$1" ) ).uniq.dup
                      end
                    when '>>' #
                      case 
                      when operand =~ /:(\w+)/    then ( vars[var] ? vars[var] - vars[$1] : [] ).dup
                      when operand =~ /\[(\w+)\]/ then ( vars[var] ? vars[var] - eval( "self.#$1" ) : [] ).dup
                      end
                    when '>>>' #
                      case
                      when operand =~ /(memo|chapter)/
                        vars[var].map{|obj| eval( "obj.#{operand}" ) rescue nil }.compact.uniq.dup
                      else
                        vars[var].map{|obj| eval( "obj.#{operand}" ) rescue nil }.compact.flatten(1).uniq.dup
                      end
                    end

      elsif /^\s*(\w+)\s*(==|>~|<~|=~|<=|>=|<|>)\s*(.*?)\s*$/.match( q ) then # -------- オブジェクト選択
        prop, operator, param = $1, $2, $3
        vars['current'] =  case operator
                           when '==' then vars['current'].select{|obj| p = eval( "obj.#{prop}" ) rescue nil; p && p == param }.compact                # 完全一致
                           when '>~' then vars['current'].select{|obj| p = eval( "obj.#{prop}" ) rescue nil; p && p.index(param) }.compact            # paramがpropに含まれる
                           when '<~' 
                             param = senddata if param == "{senddata}"
                             vars['current'].select{|obj| p = eval( "obj.#{prop}" ) rescue nil; p && param.index(p) }.compact                         # propがparamに含まれる
                           when '=~' then vars["current"].select{|obj| p = eval( "obj.#{prop}" ) rescue nil
                                                                       p && ( Regexp.new( param, Regexp::IGNORECASE ) =~ p rescue nil ) }.compact     # 正規表現
                           when '<=' then vars["current"].select{|obj| p = eval( "obj.#{prop}" ) rescue nil; p && ( p <=> param ) <= 0 }.compact      # 大小比較
                           when '>=' then vars["current"].select{|obj| p = eval( "obj.#{prop}" ) rescue nil; p && ( p <=> param ) >= 0 }.compact      # 
                           when '<'  then vars["current"].select{|obj| p = eval( "obj.#{prop}" ) rescue nil; p && ( p <=> param ) < 0 }.compact       #
                           when '>'  then vars["current"].select{|obj| p = eval( "obj.#{prop}" ) rescue nil; p && ( p <=> param ) > 0 }.compact       #
                           end
      end
    }
    
    candidates = vars["current"].dup
    puts candidates.empty? ? "No Object" : candidates.group_by{|obj| obj.class.to_s }.map{|key,val| "#{key}:#{val.length} " }


    # -------- オブジェクトの集計・表示
    first, list, count, crosscount, export, sort, order, limit, offset, keyword, jump = 
      lookup.values_at( "first", "list", "count", "crosscount", "export", "sort", "order", "limit", "offset", "keyword" ,"jump" )

    if sort then
      candidates = candidates.select{|i| eval( "i.#{sort}" ) rescue nil }.sort{|a,b| eval( "a.#{sort} <=> b.#{sort}" ) }
      candidates.reverse! if order == "dsc"
    end
    
    
    case
    when first
      candidates.uniq!
      ThinktankObject.display_format = ( first.size == 0 ? "(content)" : first )
      candidates[0].to_display
      
    when list
      candidates = candidates[ offset.to_i, limit.to_i ] if limit
      candidates.uniq!
      ThinktankObject.display_format = ( list.size == 0 ? "(id).howm | (title,30%s)" : list )
      candidates.map{|c| c.to_display }
      
    when count
      candidates.uniq!
      table = candidates.group_by{|obj| eval( "obj.#{count}" ) }
      
      case export
      when "csv"   then table.map{|key,grp| "#{key},#{grp.size}" }.join("\n")
      when "emacs" then
        tbl = table.map{|key,grp| [key,grp.length] }
        tbl = if sort == count then tbl.sort{|a,b| a[0] <=> b[0] } else tbl.sort{|a,b| a[1] <=> b[1] } end
        tbl = tbl.reverse if order == "dsc"
        tbl = tbl[ offset.to_i, limit.to_i ] if limit
        tbl.map{|i| "#{i[0]}(#{i[1]})" }.join("\n")
      else nil
      end
      
    when crosscount
      count1, count2 = crosscount.split(',')
      table1 = candidates.group_by{|obj| eval( "obj.#{count1}" ) }
      table2 = candidates.group_by{|obj| eval( "obj.#{count2}" ) }
      cross  = candidates.group_by{|obj| [ eval( "obj.#{count1}" ), eval( "obj.#{count2}" ) ] }
      
      axis1 = case count1 
              when "mon"     then ThinktankTime.mons
              when "weekday" then ThinktankTime.weekdays
              else table1.keys.compact.sort
              end
      axis2 = case count2 
              when "mon"     then ThinktankTime.mons
              when "weekday" then ThinktankTime.weekdays
              else table2.keys.compact.sort
              end
      tbl =                 [[ nil,    *axis2,                                            count1 ]]
      tbl += axis1.map{|ax1| [ ax1,    *axis2.map{|ax2| cross[[ax1,ax2]].size rescue 0 }, table1[ax1].size ] }
      tbl <<                 [ count2, *axis2.map{|ax2| table2[ax2].size },               candidates.size ]
      
      case export
      when "csv"   then tbl.map{|line| line.join(',') }.join("\n")
      when "emacs" then
        wid = tbl.flatten(1).map(&:to_s).map(&:size).max
        lin = tbl[0].map{ '-' * wid } 
        tbl = [tbl[0]] + [lin] + tbl[1..-2] + [lin] + [tbl[-1]]
        tbl.map{|rec| rec.map{|i|("%%%ds"%wid)%i}.join('|') }.join("\n")
      else nil
      end 
    end
  end

=begin
     { :query => [ "[ThinktankMemo]",
                   "content=~(orexin|Orexin|OREXIN|ORX|OX|オレキシン)",
                   "children==ThinktankChapter",
                   "title>>東京都" ]
      :first => "(content)"
      :list => "[id].howm | [created.wday] | [title]"
      :summary => "ymd,todo"
      :sort => "title", :order => "dsc", :limit => "200", :offset=>"200"
    }

     :query       : クエリ配列を実行してobjectを選ぶ
     選択クエリ
     "memo-name == thinktank-diary"                         # 完全一致
     "content =~ (orexin|Orexin|OREXIN|ORX|OX|オレキシン)"  # 正規表現
     "title >~ Orexin"                                      # 部分一致
     "name <~ DONE CANCEL POSTPONE"                         # 部分一致
     "id <~ {senddata}"                                     # senddataデータとの部分一致イテレーション
     "ymd >= 2014-01-01"                                    # 辞書順
     "ym <= 2015-01"                                        # 辞書順
     "clsid == ThinktankTime"                               # 現objectでのclass絞込み
 　　代入クエリ
     ":name << :current"                                    # 現objectをnameとして保存 ( current, ThinktankMemo, ThinktankChapter, null, all )
     ":name << :null"                                       # 現objectをnameとして保存 ( current, ThinktankMemo, ThinktankChapter, null, all )
     ":name2 >> :name1"                                     # name1からname2を除外
     ":current << [ThinktankMemo]"                          # 全指定objectを加える
     ":current >> [ThinktankMemo]"                          # 全指定objectを加える
     ":current >>> memo"                                    # 親memo/chapterの選択
     ":current >>> chapter"                                 # 親memo/chapterの選択
     ":current >>> ThinktankChapter"                        # 子objectへの選択
     ":current >>> ThinktankChapter"                        # 子objectへの選択

 　　:sort       : 全objectを指定プロパティでソートする。
 　　　:order    : 順列、逆順
 　　:first      : 最初のobjectを指定フォーマットで表示
 　　:list       : 全objectを指定フォーマットで表示
 　　　:offset   : 指定番目からを選択する
 　　　:limit    : 指定個数を選択する
 　　:count      : 指定プロパティでグループ化し計数する
 　　　:export   : 結果を指定フォーマットで表示
　　 :crosscount : ２つの指定プロパティで二重グループ化する
 　　　:export   : 結果を指定フォーマットで表示
     
=end

  def self.create_root ()
    begin
      thinktank = Marshal.load( File.open( ThinktankRoot.dumpfilepath, 'r+b' ){|f| f.read } ) 
      begin
        puts "THINKTANK-ROOT>> scanning updated memo"
        Thread.new{ thinktank.load_updated_memo }
      ensure
        puts "THINKTANK-ROOT>> memo scanning was completed"
      end
      thinktank
    rescue
      File.delete ThinktankRoot.dumpfilepath rescue nil
      thinktank = ThinktankRoot.new
      begin
        puts "THINKTANK-ROOT>> loading all memo"
        Thread.new{ thinktank.load_all_memo }.join
      ensure
        puts "THINKTANK-ROOT>> memo loading was completed"
      end
      thinktank
    end
  end



end

# ..............................................................................................................
module ThinktankFile
  include ChildOfMemo
  attr_reader :filename, :size

  def filepath () memo.dirname + filename end
  def read_content () 
    if File.exist?( filepath ) then
      begin
        File.open( filepath, 'rb:utf-8', :invalid=>:replace, :undef=>:replace ){|f| f.read }
      rescue
        File.open( filepath, 'rb:utf-16be', :invalid=>:replace, :undef=>:replace, :replace=>'?' ){|f| f.read.encode('utf-8') }
      end
    else
      puts "[#{filepath}] is not exist"
    end
  end

  def write_content! ( content ) File.open( filepath, "w" ){|f| f.write( content ) } end
  def size () File.size( filepath ) end
end

# ..............................................................................................................
class ThinktankMemo < ThinktankTime
  include ThinktankFile
  include ThinktankParent

  attr_reader :id, :content, :title, :header, :updated
  
  @@chapter_header_regexp = Regexp.new '^(\*+)[ 　\t].*$\n?'


  def self.create_memo( parent, id, content )
    content ||= ""
    filepath = if /(\d{4})\-(\d{2})\-\d{2}\-\d{6}/.match( id || Time.now.strftime( "%Y-%m-%d-%H%M%S" ) ) then
                 ThinktankRoot.memodir + ( $1 == "0000" ? "" : "#{$1}/#{$2}/" ) + "#{$&}/#{$&}.howm"
               end
    if File.exist?( filepath ) then
      puts "#{filepath} is not created, because it exists already."
      return nil

      #puts "#{filepath} is created, but it exists already."
      #return ThinktankMemo.new( parent, filepath )
    else
      puts "THINKTANK-MEMO: #{filepath} is created."
      FileUtils.makedirs( File.dirname( filepath ) )
      File.open( filepath, 'wb:utf-8' ){|f| f.write( content ); f.flush }
    end

    ThinktankMemo.new( parent, filepath )
  end

  def initialize( parent, filepath ) # ファイルからobjectを作成するが、ファイルは作成しない。

    super( parent, File.basename( filepath ) )

    @id = self.to_id
    @content = self.read_content()
    @offset = 0

    Dir::glob("#{dirname}#{@id}_????-??-??.diff").sort.reverse.each{|filepath| root << ThinktankDiff.new( self, filepath, File.mtime( filepath ) ) }
    Dir::glob("#{dirname}#{@id}_????-??-??_??-???-?.ver").sort.reverse.each{|filepath| root << ThinktankVersion.new( self, filepath, File.mtime( filepath ), filepath[-12,8] ) }
    Dir::glob("#{dirname}*").delete_if{|fn| /(diff|snap|howm|ver|bak)$/.match(fn) }.sort.reverse.each{|filepath| root << ThinktankBundled.new( self, filepath ) }

    pos, chapter_number = 0, [1]

    while chapter = @@chapter_header_regexp.match( @content, pos + 1 )
      root << ThinktankChapter.new( self, pos, chapter.begin(0) - pos, chapter_number.join('.') )
      pos, depth = chapter.begin(0), chapter[1].length
      if chapter_number[depth-1] then
        chapter_number[depth-1] = chapter_number[depth-1] + 1
        chapter_number = chapter_number[0,depth]
      else
        chapter_number[depth-1] = 1
      end
    end

    root << ThinktankChapter.new( self, pos, self.size - pos, chapter_number.join('.') ) rescue puts( "ERROR in Memo.new :: pos:#{pos}, size:#{self.size}" )

    @title = ( self.ThinktankChapter[0].title rescue puts( "#{filepath}" ) )
    @header = self.ThinktankChapter[0].header
    @updated = ThinktankTime.new( nil, File.mtime( self.filepath ) )
    self
  end

  def filename () "#{@id}.howm" end
  def dirname () root.memodir + ( @year == "0000" ? "#{@id}/" : "#{@year}/#{@month}/#{@id}/" ) end
  def size () @content.size end
  # def updated () ThinktankTime.new( nil, File.mtime( self.filepath ) ) end
end

# ..............................................................................................................
class ThinktankChapter < ThinktankTime
  include ThinktankParent
  include ChildOfMemo
  attr_reader :title, :number, :header, :address

  # for first_line analysis
  @@chapter_star = Regexp.new '^(=|\*+)[　\s]+'
  @@todo_tag     = Regexp.new '^(INBOX|TODO|SOMEDAY|WAIT|DELEGATE|PROJECT|AREA|GOAL|VISION|PURPOSE|DONE|CANCEL|POSTPONE|REFERENCE)[　\s]*'
  @@priority_tag = Regexp.new '^(\[#(A|B|C|D|)\])[　\s]+'                  # [#A]  -> #A
  @@org_tag      = Regexp.new ':[^　\s]+:[　\s]*$'                         # :aaa:bbb:ccc:
  @@link_tag     = Regexp.new '\[\[([^\[\]]+)\](\[([^\[\]]+)\])?\]'        # [[url][descript]]
  @@title_tag    = Regexp.new '(\[[^\[\]]+\])[　\s]*'                      # [xxx] -> xxx
  @@title_date_tag = Regexp.new '(\[\d{4}\-\d{2}\-\d{2}[^\[\]]*\])[　\s]*' # [xxxx-xx-xx...]

  # for following_lines analysis
  @@property_text = Regexp.new '\:PROPERTIES\:[　\s]*.*?[　\s]*\:END\:', Regexp::MULTILINE
  @@property_item = Regexp.new '^[　\s]*\:(\w+?)\:(.*)$'
  @@markup_text   = Regexp.new '#\+BEGIN(?:_(\w+))?(.*?)#\+END(_\1)?', Regexp::MULTILINE
  @@markup_item   = Regexp.new '^[　\s]*#\+(\w+)\:(.*)$'
  @@schedule_tag  = Regexp.new 'SCHEDULED:[　\s]+<(\d{4}\-\d{2}\-\d{2}[^\[\]]*?)>(--<(\d{4}\-\d{2}\-\d{2}[^\[\]]*?)>)?'
  @@deadline_tag  = Regexp.new 'DEADLINE:[　\s]+<(\d{4}\-\d{2}\-\d{2}[^\[\]]*?)>(--<(\d{4}\-\d{2}\-\d{2}[^\[\]]*?)>)?'
  @@closed_tag    = Regexp.new 'CLOSED:[　\s]+\[(\d{4}\-\d{2}\-\d{2}[^\[\]]*?)\][　\s]+(DEADLINE|SCHEDULED):'
  @@statechanged_tag = Regexp.new '\s+\- State "(\w+)"\s+from "(\w+)"\s+(\[\d{4}\-\d{2}\-\d{2}[^\[\]]*\])'  #  	 - State "DONE"       from "TODO"       [2014-11-27 木 11:35]
  @@linecreated_tag  = Regexp.new '^[　\s]*\[(\d{4}\-\d{2}\-\d{2}[^\[\]]*?)\][　\s]*(.*)$'                    #  	 [2014-11-27 木 11:35]

  def initialize( parent, offset, size, number )
    super( parent, nil )
    @offset, @size, @number = offset, size, number

    first_line, following_lines = self.content.split("\n",2)
    @title = first_line
    self.time = (( memo.ThinktankDiff.find{|ttdiff| ttdiff.contain?( @title ) } rescue nil ) || self.memo ).to_id 

    # --------------------------------------------------------------------------------------------------------------------------------------------
    # first_line analysis
    first_line.strip! rescue puts self.memo.id
    first_line.sub!( @@chapter_star, '' )
    first_line.sub!( @@todo_tag ){ root << ThinktankTodoTag.new( self, $1, @title.index(first_line) + first_line.index($1) ); '' }
    first_line.sub!( @@priority_tag ){ root << ThinktankPriorityTag.new( self, $1, @title.index(first_line) + first_line.index($1) ); '' }
    first_line.sub!( @@org_tag ){ $&.split(':').delete_if(&:empty?).each{|tag| root << ThinktankOrgTag.new( self, tag, @title.index(first_line) + first_line.index(":#{tag}:") ) }; '' }
    first_line.sub!( @@link_tag ){ root << ThinktankOrgLinkTag.new( self, $&, @title.index(first_line) + first_line.index($&) ); $1 || $2 }
    first_line.gsub!( @@title_tag ){
      m, pos = $1, @title.index(first_line)
      root << ( @@title_date_tag.match( m ) ? ThinktankChapterCreatedTime.new( self, m, nil, pos + first_line.index(m) ) : ThinktankTitleTag.new( self, m, pos + first_line.index(m) ) )
      ''
    }
    @header = first_line.strip
    
    # --------------------------------------------------------------------------------------------------------------------------------------------
    # following_lines analysis
    return unless following_lines
    following_lines.scan( @@link_tag ).each{ root << ThinktankOrgLinkTag.new( self, $&, @title.length + 1 + $`.length ) }
    following_lines.scan( @@property_text ).each{|text| 
      text.scan( @@property_item ){ root << ThinktankProperty.new( self, $1, $2.strip, $`.length + @title.length ) unless ["END", "PROPERTIES" ].include?( $1 ) }
    }
    following_lines.scan( @@markup_item ){ root << ThinktankMarkup.new( self, $1.strip, $2.strip, $`.length + @title.length ) }
    following_lines.scan( @@markup_text ){ root << ThinktankSource.new( self, $1.strip, $2.lines[0], $2.lines[1..-1].join, $`.length + @title.length ) }
    following_lines.scan( @@schedule_tag ){ root << ThinktankScheduleTag.new( self, $1, $3, $`.length + @title.length ) }
    following_lines.scan( @@deadline_tag ){ root << ThinktankDeadlineTag.new( self, $1, $3, $`.length + @title.length ) }
    following_lines.scan( @@closed_tag ){ root << ThinktankClosedTime.new( self, $1, nil, $`.length + @title.length ) }
    following_lines.scan( @@statechanged_tag ){ root << ThinktankStateChangedTime.new( self, $3, $1, $2, $`.length + @title.length ) }
    following_lines.scan( @@linecreated_tag ){ offset = $`.length + @title.length; root << ThinktankLineCreatedTime.new( self, $1, $2, $2.gsub( @@link_tag ){ $1 || $2 }, offset ) }

    @address = ( self.parentChapter ? "#{self.parentChapter.address}.#{@header}" : @header )

  end

  def nextSiblingChapter() memo.ThinktankChapter.find{|chap| chap.number === @number.next } end
  def prevSiblingChapter() memo.ThinktankChapter.find{|chap| chap.number.next === @number } end
  def siblingChapters() memo.ThinktankChapter.select{|chap| chap.number.match @number.sub(/\d+$/,'\d+$') } end
  def parentChapter() @number.split('.').size > 1 ? memo.ThinktankChapter.find{|chap| chap.number === @number.sub( /\.[0-9]+$/, '' ) } : nil end
  def childChapters() memo.ThinktankChapter.select{|chap| chap.number.match( /^#{@number}.[0-9]+$/ ) } end
  def progenyChapters() memo.ThinktankChapter.select{|chap| chap.number.index( @number ) } end

  def content () self.memo.content[ @offset, @size ] end
end


# ..............................................................................................................
class ThinktankMemoAssociatedFile < ThinktankTime
  include ThinktankFile
  # attr_reader :filename

  def initialize ( parent, filepath, tim )
    super( parent, tim )
    @filename = filepath.sub( memo.dirname, '' )
  end
end

class ThinktankDiff < ThinktankMemoAssociatedFile
  attr_reader :content

  def initialize ( parent, filepath, tim )
    super( parent, filepath, tim )
    @content = read_content()
  end

  def contain?( text ) @content.index( text ) end
end

class ThinktankVersion < ThinktankMemoAssociatedFile
  attr_reader :version, :major, :minor, :revision
  def initialize ( parent, filepath, tim, ver )
    super( parent, filepath, tim )
    @version = ver
    @major, @minor, @revision = ver.split('-')
  end
end

class ThinktankBundled < ThinktankMemoAssociatedFile
  def initialize ( parent, filepath )
    super( parent, filepath, ( File.mtime( filepath ) rescue parent.id ) )
  end
end



# ..............................................................................................................
module ChildOfChapter
  include ChildOfMemo

  def chapter ()
    if self.class == ThinktankChapter then 
      self

    elsif self.parent.class == ThinktankChapter then
      self.parent

    elsif self.parent.parent.class == ThinktankChapter then
      self.parent.parent

    end
  end

end

# ..............................................................................................................
class ThinktankTag < ThinktankObject
  include ChildOfChapter
  attr_reader :name

  def initialize( parent, name, offset )
    super( parent )
    @name, @offset = name, offset
  end
end

class ThinktankTodoTag < ThinktankTag
end
class ThinktankPriorityTag < ThinktankTag
end
class ThinktankTitleTag < ThinktankTag
end
class ThinktankOrgTag < ThinktankTag
end
class ThinktankOrgLinkTag < ThinktankTag
  attr_reader :link, :title
  def intialize( parent, name, offset )
    super( parent, name, offset )
    @link, @title = @name.split( /[\[\]]/ ).delete_if(&:empty?)
  end
end


# ..............................................................................................................
class ThinktankKeyValue < ThinktankTag
  alias :key :name
  attr_reader :value

  def initialize( parent, key, value, offset )
    super( parent, key, offset )
    @key, @value = key, value
  end
end

class ThinktankProperty < ThinktankKeyValue
end

class ThinktankMarkup < ThinktankKeyValue
end

class ThinktankSource < ThinktankMarkup
  attr_reader :header
  alias :type :key
  alias :script :value

  def initialize( parent, type, header, script, offset )
    super( parent, type, script, offset )
    @type, @header, @script = type, header, script
  end
  
end


# ..............................................................................................................
class ThinktankTimeTag < ThinktankTime
  include ChildOfChapter
  def initialize( parent, tim, tim2, offset )
    super( parent, tim, tim2 )
    @offset = offset
  end
end

class ThinktankScheduleTag < ThinktankTimeTag
end

class ThinktankDeadlineTag < ThinktankTimeTag
end

class ThinktankClosedTime < ThinktankTimeTag
end

class ThinktankLineCreatedTime < ThinktankTimeTag
  attr_reader :title, :header
  def initialize( parent, tim, title, header, offset )
    super( parent, tim, nil, offset )
    @title, @header = title, header
  end
end

class ThinktankStateChangedTime < ThinktankTimeTag
  attr_reader :pre_state, :post_state
  def initialize( parent, tim, pre_state, post_state, offset )
    super( parent, tim, nil, offset )
    @pre_state, @post_state = pre_state, post_state
  end
end

class ThinktankChapterCreatedTime < ThinktankTimeTag
end

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# サポートクラス
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
=begin
class ThinktankTable
  def initialize ( objects, *axis )  # ThinktankTable.new( self.ThinktankMemo, "year", "mon" )
    @axis = Hash[ *axis.map{|ax| [ ax, [] ] } ]
  end

  def []= ( *
          end

end
=end
