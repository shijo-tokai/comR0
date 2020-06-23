shinyServer(function(input, output) {
    
    ### 
    covid19j <- reactive({
        covid19j <-fread("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv", 
                         encoding="UTF-8")
    })
    
    output$pref <- renderUI({
        checkboxGroupInput(inputId = "prefecture", 
                           inline=T,
                           label = "都道府県選択: ", 
                           choices =  c("北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県",
                                        "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
                                        "新潟県","富山県","石川県","福井県","山梨県","長野県","岐阜県","静岡県","愛知県",
                                        "三重県","滋賀県","京都府","大阪府","兵庫県","奈良県","和歌山県",
                                        "鳥取県","島根県","岡山県","広島県","山口県",
                                        "徳島県","香川県","愛媛県","高知県",
                                        "福岡県","佐賀県","長崎県","熊本県","大分県","宮崎県","鹿児島県","沖縄県"))
    })



    output$Plot <- renderPlot({
        
        # covid19j$確定日 <- lubridate::mdy(covid19j$確定日)
        BeginDate <- ymd(20200115)
        EndDate <- Sys.Date()
        pref <- input$prefecture
        color <- c(3:(length(pref)+2))
        ip <- input$ip
        lp <- input$lp
        data <- data.frame()
        

        
        for (i in 1:length(pref)){
            
            a <- as.data.frame(expand.grid(確定日 = seq.Date(from=BeginDate, to=EndDate, by=1))) %>% 
                dplyr::left_join(covid19j() %>% 
                                     dplyr::mutate(確定日 = mdy(確定日)) %>% 
                                     dplyr::filter(居住都道府県 == pref[i]) %>%
                                     dplyr::group_by(確定日) %>%
                                     dplyr::summarise(感染者数=n()), 
                                 by="確定日") %>%
                dplyr::mutate(感染者数 = ifelse(is.na(感染者数),0,感染者数), 
                                  累積感染者数=cumsum(感染者数))
            
            names(a) = c("date","inf","cum")
            
if(ip == 8){
            
            b <- a %>%
                dplyr::mutate(lead1 = lead(inf)) %>%
                dplyr::mutate(lead2 = lead(inf,n=2))%>%
                dplyr::mutate(lead3 = lead(inf,n=3)) %>%
                dplyr::mutate(lead4 = lead(inf,n=4)) %>%
                dplyr::mutate(lead5 = lead(inf,n=5)) %>%
                dplyr::mutate(lead6 = lead(inf,n=6)) %>%
                dplyr::mutate(lead7 = lead(inf,n=7)) %>%
                dplyr::mutate(lead8 = lead(inf,n=8))  
            
            b <- b %>%
                dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8)
            
}else if(ip == 9){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) 
    
    b <- b %>%
        dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8+lead9)
}else if(ip == 10){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10))
    
    
    b <- b %>%
        dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                      +lead9+lead10)
}else if(ip == 11){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) 
    
    
    b <- b %>%
        dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                      +lead9+lead10+lead11)
}else if(ip == 12){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) 
    
    
    b <- b %>%
        dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                      +lead9+lead10+lead11+lead12)
}else if(ip == 13){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) 
    
    
    b <- b %>%
        dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                      +lead9+lead10+lead11+lead12+lead13)
}else if(ip == 14){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) 
    
    
    b <- b %>%
        dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                      +lead9+lead10+lead11+lead12+lead13+lead14)
}else if(ip == 15){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) 
    
    
    b <- b %>%
        dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                      +lead9+lead10+lead11+lead12+lead13+lead14+lead15)
}else if(ip == 16){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) 
    
    
    b <- b %>%
        dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                      +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16)
}else if(ip == 17){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) 
        
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17)
}else if(ip == 18){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) 
        
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18)
}else if(ip == 19){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) 
        
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18+lead19)
}else if(ip == 20){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) %>%
        dplyr::mutate(lead20 = lead(inf,n=20)) 
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18+lead19+lead20)
}else if(ip == 21){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) %>%
        dplyr::mutate(lead20 = lead(inf,n=20)) %>%
        dplyr::mutate(lead21 = lead(inf,n=21)) 
        
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18+lead19+lead20+lead21)
}else if(ip == 22){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) %>%
        dplyr::mutate(lead20 = lead(inf,n=20)) %>%
        dplyr::mutate(lead21 = lead(inf,n=21)) %>%
        dplyr::mutate(lead22 = lead(inf,n=22)) 
        
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18+lead19+lead20+lead21+lead22)
}else if(ip == 23){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) %>%
        dplyr::mutate(lead20 = lead(inf,n=20)) %>%
        dplyr::mutate(lead21 = lead(inf,n=21)) %>%
        dplyr::mutate(lead22 = lead(inf,n=22)) %>%
        dplyr::mutate(lead23 = lead(inf,n=23)) 
        
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18+lead19+lead20+lead21+lead22+lead23)
}else if(ip == 24){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) %>%
        dplyr::mutate(lead20 = lead(inf,n=20)) %>%
        dplyr::mutate(lead21 = lead(inf,n=21)) %>%
        dplyr::mutate(lead22 = lead(inf,n=22)) %>%
        dplyr::mutate(lead23 = lead(inf,n=23)) %>%
        dplyr::mutate(lead24 = lead(inf,n=24)) 
        
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18+lead19+lead20+lead21+lead22+lead23+lead24)
}else if(ip == 25){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) %>%
        dplyr::mutate(lead20 = lead(inf,n=20)) %>%
        dplyr::mutate(lead21 = lead(inf,n=21)) %>%
        dplyr::mutate(lead22 = lead(inf,n=22)) %>%
        dplyr::mutate(lead23 = lead(inf,n=23)) %>%
        dplyr::mutate(lead24 = lead(inf,n=24)) %>%
        dplyr::mutate(lead25 = lead(inf,n=25)) 
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18+lead19+lead20+lead21+lead22+lead23+lead24
                          +lead25)
}else if(ip == 26){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) %>%
        dplyr::mutate(lead20 = lead(inf,n=20)) %>%
        dplyr::mutate(lead21 = lead(inf,n=21)) %>%
        dplyr::mutate(lead22 = lead(inf,n=22)) %>%
        dplyr::mutate(lead23 = lead(inf,n=23)) %>%
        dplyr::mutate(lead24 = lead(inf,n=24)) %>%
        dplyr::mutate(lead25 = lead(inf,n=25)) %>%
        dplyr::mutate(lead26 = lead(inf,n=26)) 
        
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18+lead19+lead20+lead21+lead22+lead23+lead24
                          +lead25+lead26)
}else if(ip == 27){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) %>%
        dplyr::mutate(lead20 = lead(inf,n=20)) %>%
        dplyr::mutate(lead21 = lead(inf,n=21)) %>%
        dplyr::mutate(lead22 = lead(inf,n=22)) %>%
        dplyr::mutate(lead23 = lead(inf,n=23)) %>%
        dplyr::mutate(lead24 = lead(inf,n=24)) %>%
        dplyr::mutate(lead25 = lead(inf,n=25)) %>%
        dplyr::mutate(lead26 = lead(inf,n=26)) %>%
        dplyr::mutate(lead27 = lead(inf,n=27)) 
        
        
        b <- b %>%
            dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                          +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                          +lead17+lead18+lead19+lead20+lead21+lead22+lead23+lead24
                          +lead25+lead26+lead27)
}else if(ip == 28){
    b <- a %>%
        dplyr::mutate(lead1 = lead(inf)) %>%
        dplyr::mutate(lead2 = lead(inf,n=2))%>%
        dplyr::mutate(lead3 = lead(inf,n=3)) %>%
        dplyr::mutate(lead4 = lead(inf,n=4)) %>%
        dplyr::mutate(lead5 = lead(inf,n=5)) %>%
        dplyr::mutate(lead6 = lead(inf,n=6)) %>%
        dplyr::mutate(lead7 = lead(inf,n=7)) %>%
        dplyr::mutate(lead8 = lead(inf,n=8)) %>%
        dplyr::mutate(lead9 = lead(inf,n=9)) %>%
        dplyr::mutate(lead10 = lead(inf,n=10)) %>%
        dplyr::mutate(lead11 = lead(inf,n=11)) %>%
        dplyr::mutate(lead12 = lead(inf,n=12)) %>%
        dplyr::mutate(lead13 = lead(inf,n=13)) %>%
        dplyr::mutate(lead14 = lead(inf,n=14)) %>%
        dplyr::mutate(lead15 = lead(inf,n=15)) %>%
        dplyr::mutate(lead16 = lead(inf,n=16)) %>%
        dplyr::mutate(lead17 = lead(inf,n=17)) %>%
        dplyr::mutate(lead18 = lead(inf,n=18)) %>%
        dplyr::mutate(lead19 = lead(inf,n=19)) %>%
        dplyr::mutate(lead20 = lead(inf,n=20)) %>%
        dplyr::mutate(lead21 = lead(inf,n=21)) %>%
        dplyr::mutate(lead22 = lead(inf,n=22)) %>%
        dplyr::mutate(lead23 = lead(inf,n=23)) %>%
        dplyr::mutate(lead24 = lead(inf,n=24)) %>%
        dplyr::mutate(lead25 = lead(inf,n=25)) %>%
        dplyr::mutate(lead26 = lead(inf,n=26)) %>%
        dplyr::mutate(lead27 = lead(inf,n=27)) %>%
        dplyr::mutate(lead28 = lead(inf,n=28)) 
        
    
    b <- b %>%
        dplyr::mutate(den = lead1+lead2+lead3+lead4+lead5+lead6+lead7+lead8
                      +lead9+lead10+lead11+lead12+lead13+lead14+lead15+lead16
                      +lead17+lead18+lead19+lead20+lead21+lead22+lead23+lead24
                      +lead25+lead26+lead27+lead28)
}
            
            b <- b %>% 
                dplyr::mutate(num = ip*dplyr::lead(inf,n=ip+lp))
            
            b <- b %>% 
                dplyr::mutate(R0 = num/den)
            
            c <- b %>%
                dplyr::select(c("date","R0"))
            
            data <- data %>% 
                dplyr::bind_rows(data.frame(pref = pref[i], 
                                            c))
            
        }
        
        g <- ggplot()+ 
            geom_line(data = data %>% 
                          mutate(date = as.Date(as.character(date))), 
                      mapping = aes(x=date, y=R0, color=pref)) + 
            geom_hline(yintercept = 1, color="black")+
            scale_x_date(date_breaks = "7 days", date_labels = "%m/%d") + 
            ylim(0,5)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme_gray (base_family = "HiraKakuPro-W3")
        
        print(g)
    })
})
