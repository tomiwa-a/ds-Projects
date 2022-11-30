playlist_convert = function(url, type){
  if(type == "spotify"){
    #call libraries
    require(tidyverse);
    library(rvest);
    library(httr)
    library(jsonlite)
    library(httpuv)
    
    #load playlist
    page = read_html(url);
    playlist_name = page %>% html_element(".headings__title ") %>% html_text()
    playlist_description = page %>% html_element(".svelte-1bbbxe2") %>% html_text2()
    
    table = page %>% html_elements(".songs-list-row")
    title = table %>% html_elements(".songs-list__col--song") %>% html_elements(".songs-list-row__song-container") %>% html_elements(".songs-list-row__song-wrapper") %>% html_children() %>% html_children() %>% html_text2()
    
    # data frame for all songs 
    df = data.frame(name = title[seq(1,length(title), 2)], artiste = title[seq(2,length(title), 2)])
    
    #get access token among other things 
    #change to env later. 
    client_id = '0598ecfff74446c28cc4afcff17e1388'; 
    client_secret = 'ed4d27c42760499c88919ef7d79bdb71'; 
    
    enc = base64_enc(paste0(client_id,":",client_secret))
    enc = sub('\n', '', enc)
    HeaderValue = paste0('Basic ', enc)
    
    
    #Request User Authorization
    scope = 'playlist-modify-public'
    redirect_uri = "http://localhost:1410/"
    state = random::randomStrings(n=1, len=16, digits=T, loweralpha=T, unique=T, check=TRUE)
    url = paste0('https://accounts.spotify.com/authorize?response_type=code&client_id=',client_id, '&scope=', scope, '&redirect_uri=', redirect_uri, '&state=', state)
    
    r = HEAD(url)
    codes = oauth_listener(r$url)
    code = codes$code
    
    #Request Access Token
    url = "https://accounts.spotify.com/api/token"
    data = list(grant_type="authorization_code", code=code, redirect_uri=redirect_uri)
    response = POST(url = url, add_headers(.headers = c('Authorization' = HeaderValue,'Content-Type' = 'application/x-www-form-urlencoded')), body=data, encode = "form")
    r = content(response)
    
    token = r$access_token
    HeaderValue = paste0('Bearer ', token)
    
    tracks = list()
    max = nrow(df)
    i = 1
    while(i <= max){
      name = df[i, ]$name
      artiste = df[i, ]$artiste
      
      url = "https://api.spotify.com/v1/search?type=track&include_external=audio&q="
      url = URLencode(paste0(url, name))
      url = paste0(url, "%20artist:", URLencode(sub(' & ', ',', artiste)))
      url = URLencode(url)
      
      response = GET(url = url, add_headers(Authorization = HeaderValue))
      r = content(response)
      uri = r$tracks$items[1][[1]]$uri
      tracks = append(tracks, uri)
      
      i = i+ 1
    }
    
    if(length(tracks) > 0){
      
      #create playlist
      create_url = "https://api.spotify.com/v1/users/qxftahs8oz04gnktf5w290p3c/playlists"
      body = list(
        name = playlist_name,
        description = playlist_description,
        public = TRUE
      )
      response = POST(url = create_url, body = body, add_headers(.headers = c('Authorization' = HeaderValue,'Content-Type' = 'application/json')), encode = "json")
      r = content(response)
      playlist_uri = r$uri
      playlist_id = r$id
      playlist_url = r$external_urls$spotify
      #playlist_uri = "spotify:playlist:2FC3Psjjs0k7m78T7FG25r"
      #playlist_id = "2FC3Psjjs0k7m78T7FG25r"
      
      x = ceiling(length(tracks) / 100)
      i = 1
      y = 1
      while(i <= x){
        max = 100 * i 
        if(i == x){
          max = length(tracks) - (100* (i-1)) + (100*(i-1))
        }
        print(paste(y, max))
        body = list(
          uris = unlist(tracks[y:max])
        )
        url = paste0("https://api.spotify.com/v1/playlists/", playlist_id,"/tracks")
        response = POST(url = url, body = body, add_headers(.headers = c('Authorization' = HeaderValue,'Content-Type' = 'application/json')), encode = "json")
        r = content(response)
        
        i = i + 1
        y = y + 100
      }
      
      
      print(playlist_url)
    }
    
    newDf = data.frame(tracks = unlist(tracks))
    #View(newDf)
    
    
  }else{
    return("Invalid Playlist Type");
  }
}