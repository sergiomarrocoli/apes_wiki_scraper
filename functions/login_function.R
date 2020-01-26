#login
login <- "https://apeswiki.eva.mpg.de"

session <- html_session(login)

form <- html_form(read_html(url))[[1]]



GET("https://apeswiki.eva.mpg.de", authenticate(username, password))


html_session("https://apeswiki.eva.mpg.de", set_cookies('prim_panafwikiUserName' = "Wikiuser"),
             add_headers('prim_panafwikiUserName' = "Wikiuser"))





filled_form <- set_values(form,
                          username = "notmyrealemail",
                          password = "notmyrealpassword")

submit_form(session, filled_form)

