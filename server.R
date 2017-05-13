library(RMySQL)
library(ggplot2)
library(plyr)
library(plotly)
library(dplyr)
library(data.table)



diasetdb <- dbConnect(MySQL(), user = "diaset", password = "diaset00", host = "201.134.41.123", 
                      port = 3337, dbname = "diaset_dwh00")

tb_agg <- dbGetQuery(diasetdb,"SELECT * FROM fact_optime_agg2")
tb_turnos <- dbGetQuery(diasetdb,"SELECT * FROM fact_turnos_3g") 

shinyServer(function(input,output){
        ##Update the machine that correspond to each plant
        machine <- reactive({switch(
                input$plant,
                "MTY" = input$M,
                "GDL" = input$G,
                "QTO" = input$Q,
                "SALT" = input$S
                
        )})
        
        output$plot <- renderPlotly({
                
                Var_idDate<-input$date
                t1 <- input$sliderTurnos[1]
                t2 <- input$sliderTurnos[2]
                tb_agg_0sub <- subset(tb_agg,tb_agg$plant==as.character(input$plant))
                nrow(tb_agg_0sub)
                tb_agg_sub <- subset(tb_agg_0sub,tb_agg_0sub$idmachine==machine())
                nrow(tb_agg_sub)
                ##tb_agg_sub$fecha<-as.Date(tb_agg_sub$fecha)
                tb_agg_sub2<- tb_agg_sub[tb_agg_sub$fecha==as.Date(Var_idDate),]
                nrow(tb_agg_sub2)
                Hour <- tb_agg_sub2[,5]
                OpTimePerHour <- (tb_agg_sub2[,6]/3600)
                p <- ggplot(tb_agg_sub2, aes(x = Hour,y = OpTimePerHour))
                p +geom_bar(stat = "identity", fill="#009E73", colour = "black") + 
                        ggtitle("Machine Operating Time per Hour") + 
                        xlab("Hour") + ylab ("optime") + theme_classic(base_size = 14) +
                        scale_x_continuous(breaks=c(t1:t2), labels=c(t1:t2),limits=c(t1-1,t2))
                
        })
        
        ##Table that shows a summary by plant and idmachine
        output$summary <- renderTable({
                tb_agg$plant <- as.factor(tb_agg$plant)
                tb_turnos$plant <- as.factor(tb_turnos$plant)
                tb_agg$idmachine <- as.factor(tb_agg$idmachine)
                tb_turnos$idmachine <- as.factor(tb_turnos$idmachine)
                tb_agg$fecha <- as.Date(tb_agg$fecha)
                tb_turnos$fecha <- as.Date(tb_turnos$fecha)
                df <- select(tb_agg,plant:sum_machine_optime)
                
                tb_turnos_ord <- tb_turnos[,c(1,2,3,4,9,5,6,7,8)]
                df_turnos <- select(tb_turnos_ord,plant:total_hrs)
                
                idDate<-as.Date(input$date2[2])
                idDate2 <- as.Date(input$date2[1])
                ##idDate <- as.Date("2017-04-23")
                ##idDate2 <- as.Date("2017-04-17")
                
                ##new table with specific date
                df <- df[(df$fecha >= as.Date(idDate2) & df$fecha <= as.Date(idDate)),] 
                df_turnos <- df_turnos[(df_turnos$fecha >= as.Date(idDate2) & 
                                                df_turnos$fecha <= as.Date(idDate)),] 
                ##add another column to put optime in hrs round to 2 digits
                df1 <- mutate(df, optime_hr = round((sum_machine_optime/3600),2))
               
                ##Group table by plant, idmachine,fecha and summarize optime_hr
                df_3g <- df1 %>% group_by(plant,idmachine,fecha) %>% 
                        summarise (Tot_Hrs_Op = sum(optime_hr))
                
                ##Merge the column Tot_Hrs_Op from table df_3g to table df_turnos
                df_turnos <- data.table(df_turnos)
                df_3g <- data.table(df_3g)
                setkey(df_turnos,plant,idmachine,fecha)
                setkey(df_3g,plant,idmachine,fecha)
                df_3g_turnos <- merge(df_turnos, df_3g, all = TRUE)
                
                ##Group data by plant, idmachine and show table according date input in app
                df_3G <- df_3g_turnos %>% group_by(plant,idmachine) %>% 
                        summarise(Tot_Hrs_Turnos = sum(total_hrs,na.rm=TRUE), 
                                        Tot_Hrs_Op = round((sum(Tot_Hrs_Op,na.rm=TRUE)),2))
                
                ##Table with Total de Horas operando y Porcentaje de las horas operando por planta y maquina
                df_3G_1 <- mutate(df_3G, Porc_Op = round(((Tot_Hrs_Op/Tot_Hrs_Turnos)*100),2))
                
                df_3G_1
                
                
                })
        
        ##Table that shows a summary by plant
        output$summaryplanta <- renderTable({
                tb_agg$plant <- as.factor(tb_agg$plant)
                tb_turnos$plant <- as.factor(tb_turnos$plant)
                tb_agg$idmachine <- as.factor(tb_agg$idmachine)
                tb_turnos$idmachine <- as.factor(tb_turnos$idmachine)
                tb_agg$fecha <- as.Date(tb_agg$fecha)
                tb_turnos$fecha <- as.Date(tb_turnos$fecha)
                df <- select(tb_agg,plant:sum_machine_optime)
                
                tb_turnos_ord <- tb_turnos[,c(1,2,3,4,9,5,6,7,8)]
                df_turnos <- select(tb_turnos_ord,plant:total_hrs)
                
                idDate<-as.Date(input$date2[2])
                idDate2 <- as.Date(input$date2[1])
                ##idDate <- as.Date("2017-04-23")
                ##idDate2 <- as.Date("2017-04-17")
                
                ##new table with specific date
                df <- df[(df$fecha >= as.Date(idDate2) & df$fecha <= as.Date(idDate)),] 
                df_turnos <- df_turnos[(df_turnos$fecha >= as.Date(idDate2) & 
                                                df_turnos$fecha <= as.Date(idDate)),] 
                ##add another column to put optime in hrs round to 2 digits
                df1 <- mutate(df, optime_hr = round((sum_machine_optime/3600),2))
                
                ##Group table by plant, idmachine,fecha and summarize optime_hr
                df_3g <- df1 %>% group_by(plant,idmachine,fecha) %>% 
                        summarise (Tot_Hrs_Op = sum(optime_hr))
                
                ##Merge the column Tot_Hrs_Op from table df_3g to table df_turnos
                df_turnos <- data.table(df_turnos)
                df_3g <- data.table(df_3g)
                setkey(df_turnos,plant,idmachine,fecha)
                setkey(df_3g,plant,idmachine,fecha)
                df_3g_turnos <- merge(df_turnos, df_3g, all = TRUE)
                
                ##Group data by plant, idmachine and show table according date input in app
                df_3G <- df_3g_turnos %>% group_by(plant,idmachine) %>% 
                        summarise(Tot_Hrs_Turnos = sum(total_hrs,na.rm=TRUE), 
                                  Tot_Hrs_Op = round((sum(Tot_Hrs_Op,na.rm=TRUE)),2))
                
                ##Table with Total de Horas operando y Porcentaje de las horas operando por planta y maquina
                df_3G_1 <- mutate(df_3G, Porc_Op = round(((Tot_Hrs_Op/Tot_Hrs_Turnos)*100),2))
                
                ##Table with Total de Horas operando y Porcentaje de las horas operando por planta
                
                df_3G_Tot <- df_3G_1 %>% group_by(plant) %>% summarise(Tot_Hrs_Op_plant = sum(Tot_Hrs_Op,na.rm=TRUE), 
                                                Porc_Op_plant = round((mean(Porc_Op,na.rm=TRUE)),2))
                
                df_3G_Tot 
                
        })
        
        output$data <- renderTable({
           
                df <- select(tb_agg, plant:sum_machine_optime)
                idDate<-as.Date(input$date4[2])
                idDate2 <- as.Date(input$date4[1])
                ##new table with specific date
                df<- df[(df$fecha >= as.Date(idDate2) & df$fecha <= as.Date(idDate)),] 
                df1 <- mutate(df, optime_hr = sum_machine_optime/3600)
                df2 <- df1[,c(1,2,3,4,6,5)]
                df3 <- select(df2,plant:optime_hr)
                df4 <- arrange(df3, plant, idmachine,fecha,hour)
                df4
        })
        
        output$dataturnos <- renderTable({
                
                tb_turnos_ord <- tb_turnos[,c(1,2,3,4,9,5,6,7,8)]
                df_turnos <- select(tb_turnos_ord,plant:total_hrs)
                idDate<-as.Date(input$date5[2])
                idDate2 <- as.Date(input$date5[1])
                ##new table with specific date
                df_turnos1<- df_turnos[(df_turnos$fecha >= as.Date(idDate2) & 
                                             df_turnos$fecha <= as.Date(idDate)),] 
                df_turnos2 <- arrange(df_turnos1,plant,idmachine,fecha)
                df_turnos2
        })
        
}
)

