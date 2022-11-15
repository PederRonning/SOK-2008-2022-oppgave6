library(tidyverse)
library(PxWebApiData)
library(cowplot)
library(reshape2)
library(patchwork)

dframe <- ApiData("https://data.ssb.no/api/v0/no/table/12441/", 
               Kjonn=list('item', c("1", "2")), 
               Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")), 
               NACE2007=FALSE, 
               Sykefraver2=FALSE, 
               ContentsCode=TRUE)

dframe <- as_tibble(dframe[[1]])
dframe <- dframe %>% 
  select("kjønn", "år", "value") %>% 
  rename(sykefravær = value)

arbeidsledighet <- ApiData("https://data.ssb.no/api/v0/no/table/05111/", 
                        ArbStyrkStatus=list('item', c("2")), 
                        Kjonn=list('item', c("1", "2")), 
                        ContentsCode=list('item', c("Prosent")), 
                        Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")), 
                        Alder=FALSE)

arbeidsledighet <- as_tibble(arbeidsledighet[[1]])
arbeidsledighet <- arbeidsledighet %>% 
  select("kjønn", "år", "value") %>% 
  rename(arbeidsledighet = value)

dframe_arbledig <- left_join(dframe, arbeidsledighet)
dframe_arbledig$år <- dframe_arbledig$år %>% 
  as.numeric()


menn <- dframe_arbledig %>% 
  filter(kjønn == "Menn")
kvinner <- dframe_arbledig %>% 
  filter(kjønn == "Kvinner")

koeffisienten <- 0.5

kvinner_plot <-
  ggplot(kvinner, aes(x = år)) +
  geom_line(aes(y = sykefravær), color = "red") + 
  geom_line( aes(y = arbeidsledighet/koeffisienten), color = "blue") +
  scale_y_continuous(
    name = "Sykefravær %",
    sec.axis = sec_axis(~.*koeffisienten, name="Arbeidsledighet %")) +
  theme(
    axis.title.y = element_text(color = "red", size=13),
    axis.title.y.right = element_text(color = "blue", size=13)) +
  ggtitle("sykefravær og arbeidsledighet, kvinner")

koeffisient1 <- 0.7

menn_plot<-
  ggplot(menn, aes(x = år)) +
  geom_line(aes(y = sykefravær), color = "red") + 
  geom_line( aes(y = arbeidsledighet/koeffisient1), color = "blue") +
  scale_y_continuous(
    name = "Sykefravær %",
    sec.axis = sec_axis(~.*koeffisient1, name="Arbeidsledighet %")) + 
  theme(
    axis.title.y = element_text(color = "red", size=13),
    axis.title.y.right = element_text(color = "blue", size=13)) +
  ggtitle("sykefravær og arbeidsledighet, menn")

plot_grid(kvinner_plot, menn_plot)
