'' usage: unconvert infile.html [outfile.wakka]
'' (if outfile.wakka missing it outputs to screen)
dim shared as Integer LineCount=0

#ifndef __FB_VERSION4XWY__
   Function Instri(ByVal start As Integer,ByVal MainStr As String, ByVal substring As String) As Integer
      Return InStr(start,LCase(MainStr),LCase(substring))
   End Function
#EndIf

function replaceEx(byref s as string, byref oldtxt as const string, byref newtxt as const string = "", byval times as integer = 0) as integer
   '' replaceEx instances of oldtxt in s with newtxt
   dim as integer ret = 0
   assert(len(oldtxt))
   dim as integer i = instri(1,s, oldtxt)
   while i > 0
      s = left(s, i-1) & newtxt & mid(s, i + len(oldtxt))
      ret += 1
      if ret = times then exit while
      i = instri(i+len(newtxt), s, oldtxt)
   wend
   return ret
end function

function tagreplace(byref s as string, byref oldtxt as const string, byref newtxt as const string = "") as integer
   '' replaceEx instances of <oldtxt> and </oldtxt> in s with newtxt
   return replaceEx( s, "<" & oldtxt & ">", newtxt ) _
   + replaceEx( s, "</" & oldtxt & ">", newtxt )
end function

function q(byref txt as const string) as string
   '' replaceEx instances of \' with \"
   dim as string ret = txt
   replaceEx( ret, "'", """" )
   return ret
end function

function substring(byref s as const string, byval n1 as integer, byval n2 as integer) as string
   return mid(s, n1, n2-n1)
end function

function findbetween(byref s as const string, byref n1 as const string, byref n2 as const string, byref ret as string = "", byval keepn1n2 as integer = 0) as integer
   '' Find instance of n1+(...)+n2 in s, set ret equal to (...) or n1+(...)+n2 and return its position

   dim as integer p1start, p1end, p2start, p2end
   p1start = instri(1,s, n1)
   if p1start = 0 then return 0
   p1end = p1start + len(n1)

   p2start = instri(p1end, s, n2)
   if p2start = 0 then return 0
   p2end = p2start + len(n2)

   if keepn1n2 then
      ret = substring(s, p1start, p2end)
      return p1start
   else
      ret = substring(s, p1end, p2start)
      return p1end
   end if

end function

function findbetween3(byref s as const string, byref n1 as const string, byref n2 as const string, byref n3 as const string, byref ret1 as string = "", byref ret2 as string = "") as integer
   '' Find instance of n1+(...)+n2+(''')+n3 in s, set ret1/ret2 equal to (...)/(''') and return n1's position

   dim as integer p1start, p1end, p2start, p2end, p3start', p3end
   p1start = instri(1,s, n1)
   if p1start = 0 then return 0
   p1end = p1start + len(n1)

   p2start = instri(p1end, s, n2)
   if p2start = 0 then return 0
   p2end = p2start + len(n2)

   p3start = instri(p2end, s, n3)
   if p3start = 0 then return 0
   'p3end = p3start + len(n3)

   ret1 = substring(s, p1end, p2start)
   ret2 = substring(s, p2end, p3start)
   return p1start

end function

function replacebetween(byref s as string, byref n1 as const string, byref n2 as const string, byref rep as const string = "", byval keepn1n2 as integer = 0) as integer

   '' replaceEx n1+(...)+n2 in s with rep (or n1+rep+n2)

   dim as integer p1start, p1end, p2start, p2end
   p1start = instri(1,s, n1)
   if p1start = 0 then return 0
   p1end = p1start + len(n1)

   p2start = instri(p1end, s, n2)
   if p2start = 0 then return 0
   p2end = p2start + len(n2)

   if keepn1n2 then
      s = left(s, p1end-1) & rep & mid(s, p2start)
      return p1end
   else
      s = left(s, p1start-1) & rep & mid(s, p2end)
      return p1start
   end if

end function

function replacebetween3(byref s as string, byref n1 as const string, byref n2 as const string, byref n3 as const string, byref rep as string = "") as integer
   '' replaceEx n1+(...)+n2+(''')+n3 in s with rep

   dim as integer p1start, p1end, p2start, p2end, p3start, p3end
   p1start = instri(1,s, n1)
   if p1start = 0 then return 0
   p1end = p1start + len(n1)

   p2start = instri(p1end, s, n2)
   if p2start = 0 then return 0
   p2end = p2start + len(n2)

   p3start = instri(p2end, s, n3)
   if p2start = 0 then return 0
   p3end = p3start + len(n3)

   s = left(s, p1start-1) & rep & mid(s, p3end)
   return p1start

end function

function LineInput() as string
   dim as string s="",lines = ""
   dim as integer i=1,b=0
   dim as ubyte ptr ps
   do until eof(1)
      line input #1, s
      LineCount+=1
      if len(s) then
         i=instrrevi(s,"<")
         if b then
            lines &=ltrim(s)
         else
            lines &=s
         endif
         if i then
            select case lcase(mid(s,i))
               case "<a ","<b ","<br ","<link ","<span "
                  b=1
               case else
                  exit do
            End Select
         else
            exit do
         EndIf
      EndIf
   loop
   return lines
end function

function unconvert(byref infile as const string, byref outfile as const string) as integer

   dim as string s,s2, lines = ""
   dim as string origline1, origline2
   dim as string middle, middle2

   dim as integer inpre = 0
   #ifdef __FB_WIN32__
      const NEWLINE = !"\r\n"
   #else
      const NEWLINE = !"\n"
   #endif

   if open(infile for input as #1) then
      return 1
   EndIf
   do until eof(1)

      s=LineInput()

      '' Remove html/head/body tags
      replacebetween(s, "<!DOCTYPE ", ">")
      replacebetween(s, "<meta ", ">")
      replacebetween(s, "<html lang=", ">")
      replacebetween(s, "<html xmlns=", ">")
      tagreplace s, "html"
      tagreplace s, "head"
      tagreplace s, "body"
      tagreplace s, "tbody"
      replacebetween(s, "<body ", ">")

      '' Remove misceallaneous tags near start
      if findbetween(s, q("<div id="), ">", middle) then
         select case lcase(trim(middle,""""))
            case "fb_tab_l","fb_tab_r"
               replacebetween s, "<div id=" & middle & ">", "</div>"
            case "fb_body_wrapper", "fb_tab", "fb_pg_wrapper", "fb_pg_body"
               replaceEx s,  "<div id=" & middle & ">"
         End Select
      end if

      replacebetween(s, "<link rel=", q("href='style.css'>"))

      '' Title
      replaceEx s, "<title>", q("{{fbdoc item='title' value='")
      replaceEx s, q("</title>"), q("'}}----")

      '' Section headers (Eng)
      while findbetween(s, q("<div class="), ">", middle)
         middle2="<div class=" & middle & ">"
         select case lcase(trim(middle,""""))
            case "fb_sect_title"
               if findbetween(s, middle2, "</div>", middle) then
                  dim as string title
                  select case lcase(middle)
                     case "syntax", "syntaxe","语法"
                        title = "syntax"
                     case "usage","用法"
                        title = "usage"
                     case "parameters", "param&egrave;tres","参数"
                        title = "param"
                     case "return value", "valeur retourn&eacute;e","返回值"
                        title = "ret"
                     case "description","说明"
                        title = "desc"
                     case "example", "exemple","例子"
                        title = "ex"
                     case "platform differences", "diff&eacute;rences de plate-forme","平台差异"
                        title = "target"
                     case "dialect differences", "diff&eacute;rences de dialecte", "diff&eacute;rences de dialectes","方言差异"
                        title = "lang"
                     case "differences from qb", "diff&eacute;rences avec qb","与qb差别"
                        title = "diff"
                     case "see also", "voir aussi","参考"
                        title = "see"
                  end select
                  if title = "" then
                     middle = q("{{fbdoc item='section' value='" & middle & "'}}")
                  else
                     middle = q("{{fbdoc item='" & title & "'}}")
                  end if
                  replacebetween s, middle2, "</div>", title
               end if
            case "fb_header"
               if findbetween(s, middle2, "</div>", middle) then
                  replacebetween s, middle2, "</div>", "==="& middle & "==="
               End If
            case "freebasic"
               s2=s
               print LineCount,middle2
               do until instri(1,s2,"</div></tt>") or eof(1)
                  s2=trim(LineInput())
                  s &=s2
               Loop
               if replaceEx(s, "<tt>" & middle2, "%%(freebasic)" & NEWLINE)=0 then
                  if right(lines,2)="##" andalso instri(1,LTrim(s),middle2)=1  then
                     lines=left(lines,len(lines)-2)
                  endif
                  replaceEx(s, middle2, "%%(freebasic)" & NEWLINE)
               endif
               replaceEx s, "</div></tt>", "%%" & NEWLINE
            case "fb_table"
               '' Tables (all on one line)
               '' note: columns= value left blank, must be filled in manually
               '' note2: escape characters are handled differently within tables
               s2=s
               print LineCount,middle2
               do until instri(1,s2,"</table>") or eof(1)
                  s2=trim(LineInput())
                  s &=s2
               Loop
               tagreplace s, "tbody"
               if findbetween(s, "<tr>", "</tr>", middle) then
                  s2=str(SubStrCount( middle, "<td>",1))
               else
                  s2="2"
               End If
               if replaceEx(s, middle2 & "<table>", q("{{table columns='" & s2 & "' cellpadding='1' cells='")) then
                  replaceEx s, ";" '' remove semicolons from any escape chars - apparently that's how Wakka tables work
                  replaceEx (s, "</td></tr></table>", q("'}}"))
                  tagreplace s, "tr"
                  replaceEx s, "<td>"
                  replaceEx s, "</td>", ";"
               end if
            case "fb_img"
               middle2 &=q("<img src='")
               if findbetween(s, middle2, !"\"", middle) then
                  replacebetween s, middle2, ">", q("{{image url='") & middle & q("'}}")
               End If
            case "fb_sect_cont"
               replaceEx s, middle2
            case "fb_indent"
               replaceEx s, middle2, !"\t\t"
            case else
               replaceEx s, middle2, "<?!" & mid(middle2,2)
         End Select
      wend

      while findbetween(s, q("<div style="), ">", middle)
         middle2="<div style=" & middle & ">"
         select case lcase(trim(middle,""""))
            case "clear:both","clear: both","clear: both;"
               replaceEx s, middle2, "::c::"
            case else
               replaceEx s, middle2, "<?!" & mid(middle2,2)
         End Select
      wend

      '' Code examples header/footer
      'replaceEx s, q("<tt><div class='freebasic'>"), ("%%(freebasic)" & NEWLINE)
      replaceEx s, "</div></tt><br />", "%%<br />"

      '' Syntax highlighting in code examples
      while findbetween(s, q("<span class="), ">", middle)
         s2=s
         do until instri(1,s2,"</span>") or eof(1)
            s2=trim(LineInput())
            s &=s2
         Loop
         while findbetween3(s, "<span class=", ">", "</span>", middle, middle2)
            replacebetween3 s, "<span class=", ">", "</span>", middle2
         wend
      wend

      '' fbdoc links (do before <br \> processing)
      if findbetween3(s, q("<b><a href='"), q(".html'>"), q("</a></b><br \>"), middle, middle2) then
         replacebetween3 s, q("<b><a href='"), q(".html'>"), q("</a></b><br \>"), _
         q("=={{fbdoc item='keyword' value='") & middle & "|" & middle2 & q("'}}==") & NEWLINE
      end if

      '' fbdoc anchor links
      while findbetween3(s, q("<a href='#"), q("'>"), "</a>", middle, middle2)
         replacebetween3 s, q("<a href='#"), q("'>"), "</a>", _
         q("{{anchor name='") & middle & "|" & middle2 & q("'}}")
      wend

      '' fbdoc anchors (do before <b> processing)
      while findbetween3(s, q("<a name='"), q("'></a><b><u>"), "</u></b>", middle, middle2)
         replacebetween3 s, q("<a name='"), q("'></a><b><u>"), "</u></b>", _
         q("{{anchor name='") & middle & q("'}}") & _
         q("{{fbdoc item='section' value='") & middle2 & q("'}}")
      wend

      '' fbdoc anchors (do before <b> processing)
      while findbetween(s, "<a name=", "></a>", middle)
         replacebetween s, "<a name=", "></a>", _
         q("{{anchor name='") & trim(middle,"""") & q("'}}")
      wend

      '' Wiki links
      while findbetween3(s, q("<li> <a href='"), q("'>"), "</a>", middle, middle2)
         if lcase(right(middle,5))=".html" then middle=left(middle,len(middle)-5)
         replacebetween3 s, q("<li> <a href='"), q("'>"), "</a>", q("<li> {{fbdoc item='keyword' value='" & middle & "|" & middle2 & "'}}")
      wend

      '' Wiki links
      while findbetween3(s, q("<a href='"), q("'>"), "</a>", middle, middle2)
         if lcase(right(middle,5))=".html" then middle=left(middle,len(middle)-5)
         replacebetween3 s, q("<a href='"), q("'>"), "</a>", "[[" & middle & "|" & middle2 & "]]"
      wend

      if findbetween(s, "<b><u>", "</u></b>", middle) then
         replacebetween s, "<b><u>", "</u></b>", q("{{fbdoc item='section' value='" & middle & "'}}")
      end if
      '' <pre>
      if instr(s, "pre") then
         if instr(s, "</pre") then
            inpre = 0
         elseif instr(s, "<pre") then
            inpre = 1
         end if
         replaceEx s, q("<pre class='fb_pre'>"), "%%"
         replaceEx s, "</pre>", "%%"
      end if


      '' Tables (<<>>)
      replaceEx s, q("<table class='fb_box'>"), "<<"
      replaceEx s, "</div></td></tr></table>", ">>"
      replaceEx s, "</table>", ">>"
      replaceEx s, "</div></td><td>", "<<>>"

      tagreplace s, "td"
      tagreplace s, "tr"


      '' Unprocessed newline tags
      replaceEx s, "<div>"
      replaceEx s, "</div>", NEWLINE
      replaceEx s, "<br/>", NEWLINE
      replaceEx s, "<br />", NEWLINE
      replaceEx s, "<br \="""">", NEWLINE
      replaceEx s, "<br \>", NEWLINE ''(bad emitter?)


      '' Formatting tags
      tagreplace s, "b", "**"
      tagreplace s, "i", "//"
      tagreplace s, "tt", "##"
      tagreplace s, "u", "__"
      tagreplace s, "ul"
      replaceEx s, "<li>", !"\t-"
      replaceEx s, "</li>"


      '' Escapes
      replaceEx s, "&nbsp;", " "
      replaceEx s, "&amp;", "&"
      replaceEx s, "&gt;", ">"
      replaceEx s, "&lt;", "<"


      '' add to output lines
      lines &= rtrim(s, any !"\t ")
      if inpre then lines &= NEWLINE

   loop
   close #1

   '' Output file
   if len(outfile) then

      if open(outfile for output as #1) then return 2
      print #1, lines
      close #1
   else
      print lines
   end if

   return 0

end function

function main() as integer
   dim as string infile = command(1), outfile = command(2)
   'print "unconvert " & infile & " " & outfile
   Return unconvert( infile, outfile )

   'infile = dir("FBChs\*.html")
   'do while len(infile)

   '   outfile = left(infile, instrrev(infile, ".")-1)

   '   outfile = "CacheChs\" & outfile & ".wakka"
   '   print outfile
   '   LineCount=0
   '   unconvert( "FBChs\"& infile, outfile )
   '   infile = dir()
   'loop
end function

end main()
