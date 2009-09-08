{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Papps.Views where

import HSP
import Happstack.Server
import Happstack.Server.HSP.HTML
import Text.Highlighting.Kate
import Text.XHtml.Strict hiding (header, linesToHtml)
import System.Locale
import System.Time 

import Papps.State

linesToHtml :: String -> [SourceLine] -> String
linesToHtml vlang lns = 
  renderHtmlFragment $ formatAsXHtml [OptNumberLines] vlang lns

dateStr :: ClockTime -> String
dateStr ct =
  formatCalendarTime
    defaultTimeLocale
    "%Y-%m-%d %H:%M:%S"
    (toUTCTime ct)

homeView :: String -> Maybe PasteEntry -> ServerPartT IO Response
homeView selval pe = 
    ret $ 
      <div class="content-block">
        <form action="/addPaste" method="POST">
          <span>
            <label for="lang">Language: </label>
            <% tselect %><br />
            <label for="code">Code:  </label><br />
            <% tarea %><br />
            <input type="submit" class="element-submit" value="Submit" />
          </span>
        </form>
      </div>
  where (tselect, tarea) = 
          case pe of 
            Just  f -> (langSelect (pasteLang f), ta (pasteRaw f))
            Nothing -> (langSelect selval, ta "")
        ta t = <textarea name="code" class="element-style"  rows="15" cols="70"><% t %></textarea>

pasteView :: PasteEntry -> ServerPartT IO Response
pasteView f = 
  ret $
    <div>
      <div class="block">
        <div class="block-header">
          <span class="block-link">
            <% pasteLang f %> 
          </span>
        </div>
        <div class="code-block">
          <% cdata $ linesToHtml (pasteLang f) (pasteLines f) %>
        </div>
        <a class="simple-link" href=("/" ++ (pasteURL f) ++ "/raw")>Raw</a>
        <a class="simple-link" href=("/" ++ (pasteURL f) ++ "/dl")>Download</a>
        <a class="simple-link" href=("/" ++ (pasteURL f) ++ "/fork")>Fork</a>
        <span class="date-submit">
          <% (dateStr . pasteCreatedDate) f %>
        </span>
      </div>
      <div>
        Comments: <br />
        <% map renderComment (pasteComments f) %>
      </div>
      <% commentForm $ pasteURL f%>
      </div>
            

recentView :: [PasteEntry] -> ServerPartT IO Response
recentView ps = 
  ret $ 
    if length ps == 0 
      then 
        <div class="block">
          <p>No pastes exist!</p>
          <p><a href="/paste">Get started pasting</a></p>
        </div>
      else 
        <div><% map renderEntry ps %></div>

renderEntry :: PasteEntry -> HSP XML
renderEntry f = 
  <div class="block">
    <span class="block-header">
      <span class="block-link"><% pasteLang f %></span>
    </span>
    <div class="code-block">
      <% cdata $ linesToHtml (pasteLang f) (take 4 $ pasteLines f) %>
    </div>
    <a class="simple-link" href=(pasteURL f)>View</a>
    <span class="date-submit">
      <% (dateStr . pasteCreatedDate) f %>
    </span>
  </div>

commentForm :: String -> HSP XML
commentForm url = 
  <div class="block">
    <form action=("/" ++ url ++ "/addComment") method="POST">
      <textarea name="comments" class="element-style" rows="5" cols="70" ></textarea><br />
      <input type="submit" type="submit" class="element-style" value="Submit" />
    </form>
  </div>

renderComment :: Comment -> HSP XML
renderComment c = 
  <div class="block">
    <div class="date-submit">
      <% (dateStr . commentCreatedDate) c %>
    </div>
    <div class="code-block">
      <% commentComment c %>
    </div>
  </div>

pasteError :: HSP XML -> ServerPartT IO Response
pasteError e = ret $
              <div class="block">
                <p><% e %></p>
              </div>

header :: HSP XML
header = 
  <div id="header">
    <ul id="menu">
      <li><a href="/paste">Paste</a></li>
      <li><a href="/recent">Recent</a></li>
      <li id="title">Papps</li>
    </ul>
  </div>

footer :: HSP XML
footer = 
  <div id="footer">
    <a href="http://github.com/teneighty/Papps/">git papps</a>
  </div>

langSelect :: String -> HSP XML
langSelect val = 
    <select name="lang" class="element-style">
    <% map langOption languages %>
    </select>
  where
    langOption x = if x == val
                    then <option value=x selected=True><% x %></option>
                    else <option value=x><% x %></option>

wrapper :: HSP XML -> HSP XML
wrapper htmlBody = 
  withMetaData html4Strict $
  <html>
    <head>
      <title>Paste & View</title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <style type="text/css"><% defaultHighlightingCss %></style>
      <link href="/styles/screen.css" rel="stylesheet" type="text/css" />
      <link href="/styles/custom.css" rel="stylesheet" type="text/css" />
    </head>
    <body>
      <div id="container">
        <% header %>
        <div id="content">
          <% htmlBody %>
        </div>
      <% footer %>
      </div>
    </body>
  </html>

ret :: HSP XML -> ServerPartT IO Response
ret = (webHSP . wrapper)

badUrl, noCode, fixComments :: HSP XML
badUrl = <p>Dude! That was a boogus url. Not cool.</p>
noCode = <p>Dude! You didn't paste any code!</p>
fixComments = <p>Fix the comments</p>

