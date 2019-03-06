rm(list = ls())
library(xml2)
library(rvest)
library(tidyverse)
library(stringr)
library(magrittr)

####LJ----
i<-1
lj_inf <- tibble()
for (i in 1:67){
  web<- read_html(paste("https://bj.lianjia.com/zufang/pg",i,sep=""),
                  encoding="UTF-8")
  lj_tags <- web %>% 
    html_nodes('p.content__list--item--des') %>% 
    html_text()
  lj_tags <- str_split_fixed(lj_tags,"/",5)  
  lj_tags <- gsub('\n','',lj_tags)
  lj_tags <- gsub(' ','',lj_tags)
  lj_loc <- str_split_fixed(lj_tags[,1],"-",2)
  lj_tp <- web %>% 
    html_nodes('span.content__list--item-price') %>% 
    html_text()
  lj_tp <- as.numeric(gsub('元.*','',lj_tp))
  lj_size <- as.numeric(gsub('㎡.*','',lj_tags[,2]))
  lj_up <- lj_tp/lj_size
  lj <- tibble('Territory'=lj_loc[,1],
               'Location'=lj_loc[,2],
               'TotalPrice'=lj_tp,'UnitPrice'=lj_up,
               'Type'=lj_tags[,4],'Area'=lj_size)
  lj_inf <- rbind(lj_inf,lj)
}

####AJK----
rm(list = ls())
i <- 1
ajk_inf <- tibble()
for (i in 1:50) {
  web<- read_html(paste("https://bj.zu.anjuke.com/fangyuan/l1-p",i,"-x1/",sep=""),
                  encoding="UTF-8")
  ajk_tags <- web %>% 
    html_nodes('p.details-item.tag') %>% 
    html_text()
  ajk_tags <- gsub('\n','',ajk_tags)
  ajk_tags <- str_split_fixed(ajk_tags,'\\|',3)
  ajk_loc <- web %>% 
    html_nodes('address.details-item') %>% 
    html_text()
  ajk_loc <- gsub('.*\n','',ajk_loc)
  ajk_loc <- str_split_fixed(ajk_loc,'-',2)
  ajk_loc1 <- str_split_fixed(ajk_loc[,2],' ',2)
  ajk_tp <- web %>% 
    html_nodes('div.zu-side') %>% 
    html_text()
  ajk_tp <- as.numeric(gsub('\n|元.*','',ajk_tp))
  ajk_size <- as.numeric(gsub('平.*','',ajk_tags[,2]))
  ajk_up <- ajk_tp/ajk_size
  ajk <- tibble('Territory'=gsub('.* ','',ajk_loc[,1]),
                'Location'=ajk_loc1[,1],
               'TotalPrice'=ajk_tp,'UnitPrice'=ajk_up,
               'Type'=gsub('.* ','',ajk_tags[,1]),'Area'=ajk_size)
  ajk_inf <- rbind(ajk_inf,ajk)
}

####合并----
#LJ的数据房型中有“卫”，AJK中没有，该步骤统一二者房型
lj_inf1 <- lj_inf
lj_inf1[,5] <- gsub('.卫','',lj_inf$Type)
rent_inf <- rbind(lj_inf1,ajk_inf)

####数据处理----
rent_inf <- unique(rent_inf)
#去掉地区是北京周边的
rent_inf <- rent_inf[-(which(rent_inf$Territory=='北京周边')),]
#以Territory为分类做均值
ter_mean <- aggregate(cbind(rent_inf$TotalPrice,rent_inf$UnitPrice,rent_inf$Area),
          list(rent_inf$Territory),mean)
colnames(ter_mean) <- c('Territory','TotalPrice','UnitPrice','Area')
head(ter_mean)

##以下以Location为分类做均值
#清洗location列中同义地区
rent_inf2 <- rent_inf
rent_inf2$Location <- gsub('朝阳公园.','朝阳公园',rent_inf2$Location)
rent_inf2$Location <- gsub('黄村.','黄村',rent_inf2$Location)
rent_inf2$Location <- gsub('望京.','望京',rent_inf2$Location)
rent_inf2$Location <- gsub('天宫院.','天宫院',rent_inf2$Location)
rent_inf2$Location <- gsub('瀛海.','瀛海',rent_inf2$Location)
rent_inf2$Location <- gsub('朝阳门.','朝阳门',rent_inf2$Location)
rent_inf2$Location <- gsub('东直门.','东直门',rent_inf2$Location)
rent_inf2$Location <- gsub('建国门.','建国门',rent_inf2$Location)
rent_inf2$Location <- gsub('看丹桥.','看丹桥',rent_inf2$Location)
rent_inf2$Location <- gsub('六里桥.','六里桥',rent_inf2$Location)
rent_inf2$Location <- gsub('右安门.','右安门',rent_inf2$Location)
rent_inf2$Location <- gsub('公主坟.','公主坟',rent_inf2$Location)
rent_inf2$Location <- gsub('玉泉路.','玉泉路',rent_inf2$Location)
rent_inf2$Location <- gsub('九棵树.*','九棵树',rent_inf2$Location)
rent_inf2$Location <- gsub('广安门.','广安门',rent_inf2$Location)
rent_inf2$Location <- gsub('西直门.','西直门',rent_inf2$Location)
#对平谷做区域化处理
rent_inf2$Location[which(rent_inf2$Territory=="平谷")] <- '平谷'
#去掉location数量小于10的行
loc_num <- as.data.frame(sort(table(rent_inf2$Location)))
loc_del <- as.character(loc_num[loc_num$Freq<10,1])
rent_inf2 <- rent_inf2[!(rent_inf2$Location %in% loc_del),]
loc_mean <- aggregate(cbind(rent_inf2$TotalPrice,rent_inf2$UnitPrice,rent_inf2$Area),
                      list(rent_inf2$Territory,rent_inf2$Location),mean)
colnames(loc_mean) <- c('Territory','Location','TotalPrice','UnitPrice','Area')
head(loc_mean)


####数据展示----
library(ggplot2)

windowsFonts(myFont = windowsFont("微软雅黑"))
#柱状图
png("col.png",width=3000,height=2000,res=300)
ggplot(data = ter_mean,
       aes(x=reorder(ter_mean$Territory,ter_mean$UnitPrice),
           y=ter_mean$UnitPrice))+
  geom_col(fill= "pink")+
  theme(legend.position = 'none',
        axis.text.x = element_text(family = 'myFont',size = 10,face = 'bold',
                                   angle = 45,vjust = 1,hjust = 1),
        axis.title = element_text(family = 'myFont',size = 14,face = 'bold'),
        axis.text.y = element_text(size = 10,face = 'bold'),
        panel.grid.major.x=element_blank())+
  scale_y_continuous(breaks=seq(0,150,25))+
  geom_text(aes(label = round(UnitPrice,digits = 1)),size=3)+
  labs(x='Territory',y='Unit Price',
       title = 'Average unit price for rental house in BJ')
dev.off()
#箱图
png("boxplot.png",width=3000,height=2000,res=300)
ggplot(data = rent_inf,
       aes(x=reorder(Territory,UnitPrice,FUN = median),y=UnitPrice,
           color=Territory))+
  geom_boxplot()+
  theme(legend.position = 'none',
        axis.text.x = element_text(family = 'myFont',size = 10,face = 'bold',
                                   angle = 45,vjust = 1,hjust = 1),
        axis.title = element_text(family = 'myFont',size = 14,face = 'bold'),
        axis.text.y = element_text(size = 10,face = 'bold'),
        panel.grid.major.x=element_blank())+
  scale_y_continuous(breaks=seq(0,400,50))+
  labs(x='Territory',y='Unit Price',
       title = 'Average unit price for rental house in BJ')
dev.off()