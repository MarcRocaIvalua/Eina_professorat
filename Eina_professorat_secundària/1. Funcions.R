
# Colors i theme Ivàlua ----

paleta_ivalua <- c("#002C4B", "#40617a", "#4A5C83", "#6782a1",
                   "#74b2c0", "#81c6d0","#bcdfe4", "#e5e4e4",
                   "#fac5c5", "#d72132", "#C0001B")

library(extrafont)
loadfonts(quiet = TRUE)

theme_ivalua <- function(base_size = 11,
                         base_family = "Roboto",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22,
                         axis_text_x = TRUE,
                         axis_text_y = FALSE,
                         axis_titles = FALSE) {
  t <- ggplot2::theme_classic(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) + # need to eventually change to replace()
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = base_family,
        face = "bold",
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        family = base_family,
        face = "plain"
      ),
      plot.caption = ggplot2::element_text(
        family = base_family,
        size = base_size + 1,
        hjust = 0,
        vjust = 1.2,
        face = "plain"
      ),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text.x = ggplot2::element_text(
        face = "bold",
        size = 14, 
        angle = 0
      ),
      strip.text.y.left = ggplot2::element_text(
        face = "bold",
        size = 14,
        angle = 0
      ),
      strip.text.y.right = ggplot2::element_text(
        face = "bold",
        size = 14,
        angle = 0
      ),
      axis.text.x = ggplot2::element_text(
        family = base_family,
        face = "plain",
        size = base_size
      ),
      axis.text.y = ggplot2::element_text(
        family = base_family,
        face = "plain",
        size = base_size
      ),
      axis.title.x = ggplot2::element_text(
        family = base_family,
        face = "plain"
      ),
      axis.title.y = ggplot2::element_text(
        family = base_family,
        face = "plain"
      )
    )
  if(!axis_titles){
    t <- t +
      theme(axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank())
  }
  if(!axis_text_y){
    t <- t +
      theme(axis.text.y = ggplot2::element_blank())
  }
  if(!axis_text_x){
    t <- t +
      theme(axis.text.x = ggplot2::element_blank())
  }
  return(t)
}


# Dades MPNP ----


panell <- read_excel("Secundària_dades_eina_correcció.xlsx") %>%
  mutate(nivell = "ESO i batxillerat") %>% 
  bind_rows(read_excel("Primària_dades_eina.xlsx") %>% 
              mutate(nivell = "Infantil i primària")) %>% 
  mutate(curs_character = paste0(as.character(curs), "-", as.character(curs + 1))) %>% 
  mutate(especialitat = case_when(
    
    # ESO i batxillerat
    
    especialitat == "ADC" ~ "Aula d'acollida",
    especialitat == "AN" ~ "Anglès",
    especialitat == "CLA" ~ "Cultura clàssica",
    especialitat == "CN" ~ "Biologia i geologia",
    especialitat == "DI" ~ "Dibuix",
    especialitat == "ECO" ~ "Economia",
    especialitat == "EF" ~ "Educació física",
    especialitat == "FI" ~ "Filosofia",
    especialitat == "FQ" ~ "Física i química",
    especialitat == "GE" ~ "Geografia i història",
    especialitat == "LC" ~ "Llengua catalana i literatura",
    especialitat == "LE" ~ "Llengua castellana i literatura",
    especialitat == "MA" ~ "Matemàtiques",
    especialitat == "MU" ~ "Música",
    especialitat == "PSI" ~ "Orientació educativa",
    especialitat == "SLE" ~ "Segona llengua estrangera",
    especialitat == "TEC" ~ "Tecnologia",
    especialitat == "UES" ~ "Unitat d'educació especial",
    
    # Infantil i primària
    
    especialitat == "AAP" ~ "Aula d'acollida (infantil i primària)",
    especialitat == "ALL" ~ "Audició i llenguatge",
    especialitat == "EES" ~ "Pedagogia terapèutica",
    especialitat == "INF" ~ "Educació infantil",
    especialitat == "PAN" ~ "Anglès (infantil i primària)",
    especialitat == "PEF" ~ "Educació física (infantil i primària)",
    especialitat == "PMU" ~ "Música (infantil i primària)",
    especialitat == "PRI" ~ "Educació primària",
    especialitat == "UEE" ~ "Unitat d'educació especial (infantil i primària)",
  ))

panell_tot <- panell %>% 
  group_by(curs, ratio, escenari_entrada,
           escenari_alumnes,concertada_privada,
           nivell) %>% 
  summarise(
    across(.cols = c(personal_disponible, personal_necessari),
           .fns = sum))

