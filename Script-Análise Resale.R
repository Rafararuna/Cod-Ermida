#O im�vel foi avaliado?

df <- data.frame("O Im�vel foi Avaliado?" = c("SIM", "N�O"),
                 Quantidade = c((nrow(base_pages) - sum(is.na(base_pages$VALOR_AVL))), 
                                sum(is.na(base_pages$VALOR_AVL))))
colnames(df)[1] <- "O Im�vel foi Avaliado?"


#Estados Atingidos pelo Resale

df2 <- data.frame(table(base_pages$ESTADO))
colnames(df2)[1] <- "Estado"
colnames(df2)[2] <- "Quantidade"


#Quantidade de Anunciante e media de desagio

base_pages$ANUNCIANTE <- ifelse(is.na(base_pages$ANUNCIANTE), "OUTROS", base_pages$ANUNCIANTE)
df3 <- data.frame(table(base_pages$ANUNCIANTE))
base_pages2 <- base_pages %>% drop_na(DESAGIO)
a <- base_pages2 %>% group_by(ANUNCIANTE) %>% summarise(mean(DESAGIO), na.rm=T)
df3[,3] <- a[,2]
names(df3) <- c("Anunciante", "Quantidade", "M�dia de Des�gio")


#Situa��o do Im�vel e m�dia de desagio

df4 <- data.frame(table(base_pages$OCUPACAO))
base_pages2 <- base_pages %>% drop_na(DESAGIO)
b <- base_pages2 %>% group_by(OCUPACAO) %>% summarise(mean(DESAGIO), na.rm=T)
df4[,3] <- b[,2]
names(df4) <- c("Situa��o do Im�vel", "Quantidade", "M�dia de Des�gio")


#Numero de cidades e Cidade em destaque

length(unique(base_pages$CIDADE))

df5 <- data.frame(table(base_pages$CIDADE))
names(df5) <- c("Destaque para as Cidades", "Quantidade")
df6 <- df5 %>% arrange(desc(Quantidade)) %>% filter(Quantidade >= 50) #colocar a quantidade de habitantes por cidade


#Tipos de Venda e m�dia de Desagio

df7 <- data.frame(table(base_pages$TIPO_VENDA))
base_pages2 <- base_pages %>% drop_na(DESAGIO)
c <- base_pages2 %>% group_by(TIPO_VENDA) %>% summarise(mean(DESAGIO), na.rm=T)
df7[,3] <- c[,2]
names(df7) <- c("Tipos de Venda", "Quantidade", "M�dia de Des�gio")

base_pages3 <- base_pages %>% filter(is.na(DESAGIO))
df8 <- data.frame(table(base_pages3$TIPO_VENDA))
d <- base_pages3 %>% group_by(TIPO_VENDA) %>% summarise(mean(DESAGIO), na.rm=T)
df8[,3] <- d[,2]
names(df8) <- c("Tipos de Venda", "Quantidade", "M�dia de Des�gio")


#Numero de fotos do imovel ou do mapa por Anunciante e m�dia de desagio

base_pages2 <- base_pages %>% drop_na(DESAGIO)
base_pages2$STATIC <- ifelse(str_detect(base_pages2$LINK_FOTO, 'static'), "Sem foto","Foto do im�vel/mapa")
base_pages2$REF <- 1
df9 <- base_pages2 %>% group_by(ANUNCIANTE, STATIC) %>% summarise(sum(REF), mean(DESAGIO))
names(df9) <- c("Anunciante", "Foto do An�ncio", "Quantidade", "M�dia de Des�gio")


#Tipos de venda por Anunciante e media de desagio

base_pages2 <- base_pages %>% drop_na(DESAGIO)
base_pages2$REF <- 1
df10 <- base_pages2 %>% group_by(ANUNCIANTE, TIPO_VENDA) %>% summarise(sum(REF), mean(DESAGIO))
names(df10) <- c("Anunciante", "Tipo de Venda", "Quantidade", "M�dia de Des�gio")