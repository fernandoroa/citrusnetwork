server <- function(input, output, session) {
  output$karyoUI <- renderUI({
    fluidRow(
      column(
        6,
        selectizeInput(
          inputId = "karyotypeInput",
          label = inputTitle,
          choices = my_list,
          options = list(render = I(
            '{
                           item: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           },
                           option: function(item, escape) {
                           return "<div>" + item.label + "</div>"
                           }
                           }'
          ))
        )
      )
    )
  })

  my_list <-
    list(
      `<i>Citrus iwaikan</i> / Iwaikan` = "Citrus iwaikan / Iwaikan",
      `<i>Citrus natsukiu</i>` = "Citrus natsukiu",
      `<i>Citrus longispina</i> / Winged lime` = "Citrus longispina / Winged lime",
      `<i>Citrus webberi</i> / Kalpi` = "Citrus webberi / Kalpi",
      `<i>Citrus hystrix</i>` = "Citrus hystrix",
      `<i>Citrus tamurana</i> / A036 Hyuganatsu` = "Citrus tamurana / A036 Hyuganatsu",
      `<i>Citrus komikan</i> / A077(A076) Komikan` = "Citrus komikan / A077(A076) Komikan",
      `<i>Citrus bergamia</i> / A006-Bergamot` = "Citrus bergamia / A006-Bergamot",
      `<i>Citrus macrophylla</i> / Alemow` = "Citrus macrophylla / Alemow",
      `<i>Citrus ujukitsu</i> / A197 Ujukitsu` = "Citrus ujukitsu / A197 Ujukitsu",
      `<i>Citrus obovoidea</i> / A055 Kinkoji` = "Citrus obovoidea / A055 Kinkoji",
      `<i>Citrus taiwanica</i> / A094 (A030) Nansho daidai` = "Citrus taiwanica / A094 (A030) Nansho daidai",
      "--- other kar. ---",
      "curr. rep. true species",
      `<i>Citrus limon</i> / A085 Lemon` = "Citrus limon / A085 Lemon",
      `<i>Citrus aurantium</i> / A141 Sour orange` = "Citrus aurantium / A141 Sour orange"
    )

  output$karyoUI3 <- renderUI({
    tagList(
      htmlOutput("karyoLegend"),
      uiOutput("karyoReference")
    )
  })
  output$k4Legend <- renderUI({
    tagList(
      htmlOutput("k4Text"),
    )
  })
  output$k3Legend <- renderUI({
    tagList(
      htmlOutput("k3Text"),
    )
  })

  output$karyoLegend <- renderUI(HTML(
    values$legend
  ))


  References <- reactive({
    if (length(values$references)) {
      ref <- "References"
    } else {
      ref <- ""
    }
  })

  output$karyoReference <- renderUI({
    tagList(
      h3(References()),
      HTML(paste(values$references, collapse = "<br></br>"))
    )
  })

  values <- reactiveValues(vertexNamesdf = objectList$vertexNamesdf, legendYcoord = 0, spNumber = 3, refNumber = 1)

  output$karyoUI2 <- renderUI({
    req(input$karyotypeInput)
    tagList(
      imageOutput("karyoPlot",
        height = (values$spNumber * 3) * divisor
      )
    )
  })

  output$k4image <- renderUI({
    imageOutput("k4plot", height = plotheight * divisor)
  })

  output$k3image <- renderUI({
    imageOutput("k3plot", height = plotheight * divisor)
  })

  observeEvent(input$checkboxk4, {
    updateCheckboxInput(
      session = session,
      inputId = "checkboxk3",
      value = isolate(input$checkboxk4)
    )
  })

  observeEvent(input[["checkboxk3"]], {
    updateCheckboxInput(
      session = session,
      inputId = "checkboxk4",
      value = isolate(input$checkboxk3)
    )
  })



  observe({
    req(input$karyotypeInput)
    if (input$karyotypeInput %in% "Citrus iwaikan / Iwaikan") {
      values$dfChrSize <- objectList$threeIwaikanSize
      values$dfMarkPos <- objectList$threeIwaikanMarkPos
      values$dfMarkColor <- objectList$markStylethreeIwaikanDF
      values$leftNotes <- objectList$leftNotesIwaikandf
      values$notes <- objectList$notesIwaikandf

      values$moveKarHor <- NA
      values$anchorText <- NA
      values$anchorLineLty <- NA
      values$addMissingOTUAfter <- NA
      values$anchorTextMParental <- NA
      values$anchor <- F
      values$orderChr <- "chrNameUp"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. iwaikan"),
        "and current representatives of its ancestors (Wu et al. 2014). Underlined chr. represent pairs.
                                  Hom: Homeologous chr. as determined by cited authors"
      )

      values$references <- refStrings[which(names(refStrings) %in% c("Guerra2020", "Wu2014c"))]
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 3
      values$refNumber <- 2
    } else if (input$karyotypeInput %in% "Citrus natsukiu") {
      values$dfChrSize <- objectList$threeNatsukiuSize
      values$dfMarkPos <- objectList$threeNatsukiuMarkPos
      values$dfMarkColor <- objectList$markStylethreeNatsukiuDF
      values$leftNotes <- objectList$leftNotesNatsukiudf
      values$notes <- NA
      values$moveKarHor <- NA
      values$anchorText <- NA
      values$anchorLineLty <- NA
      values$addMissingOTUAfter <- NA
      values$anchorTextMParental <- NA
      values$anchor <- F
      values$orderChr <- "original"
      values$legend <- paste("Figure.", em("C. natsukiu"), "2n = 18")
      values$references <- character()
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 1.5
      values$refNumber <- 0
    } else if (input$karyotypeInput %in% "Citrus longispina / Winged lime") {
      values$dfChrSize <- objectList$threeLongispinaSize
      values$dfMarkPos <- objectList$threeLongispinaMarkPos
      values$dfMarkColor <- objectList$markStylethreeLongispinaDF
      values$leftNotes <- objectList$leftNotesLongispinadf
      values$notes <- objectList$notesLongispinadf
      values$moveKarHor <- NA
      values$anchorText <- NA
      values$anchorLineLty <- NA
      values$addMissingOTUAfter <- NA
      values$anchorTextMParental <- NA
      values$anchor <- F
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. longispina"),
        "and current representatives of its ancestors (Ramadugu et al. 2013). Underlined chr. represent pairs."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Ramadugu2013", "Guerra2020", "Yamamoto_2007"))]
      values$legendYcoord <- 5
      values$groupName <- F
      values$spNumber <- 3
      values$refNumber <- 3
    } else if (input$karyotypeInput %in% "Citrus webberi / Kalpi") {
      values$dfChrSize <- objectList$threeWebberiSize
      values$dfMarkPos <- objectList$threeWebberiMarkPos
      values$dfMarkColor <- objectList$markStylethreeWebberiDF
      values$leftNotes <- objectList$leftNotesWebberidf
      values$notes <- objectList$notesWebberidf
      values$moveKarHor <- NA
      values$anchorText <- NA
      values$anchorLineLty <- NA
      values$addMissingOTUAfter <- "C. micrantha"
      values$missOTUspacings <- 1
      values$anchorTextMParental <- NA
      values$anchor <- F
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. webberi"),
        "and current representative of its main ancestor (Ramadugu et al. 2013)."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Ramadugu2013", "Yamamoto_2007"))]
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 2.5
      values$refNumber <- 2
    } else if (input$karyotypeInput %in% "Citrus hystrix") {
      values$dfChrSize <- objectList$threeHystrixSize
      values$dfMarkPos <- objectList$threeHystrixMarkPos
      values$dfMarkColor <- objectList$markStylethreeHystrixDF
      values$leftNotes <- objectList$leftNotesHystrixdf
      values$notes <- objectList$notesHystrixdf
      values$moveKarHor <- NA
      values$anchorText <- NA
      values$anchorLineLty <- NA
      values$addMissingOTUAfter <- "C. micrantha"
      values$missOTUspacings <- 1
      values$anchorTextMParental <- NA
      values$anchor <- F
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. hystrix"),
        "and current representative of its main ancestor (Nicolosi 2007)."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Nicolosi2007", "Yamamoto_2007"))]
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 2.5
      values$refNumber <- 2
    } else if (input$karyotypeInput %in% "Citrus macrophylla / Alemow") {
      values$dfChrSize <- objectList$threeMacrophyllaSize
      values$dfMarkPos <- objectList$threeMacrophyllaMarkPos
      values$dfMarkColor <- objectList$markStylethreeMacrophyllaDF
      values$leftNotes <- objectList$leftNotesMacrophylladf
      values$notes <- objectList$notesMacrophylladf
      values$moveKarHor <- "C. macrophylla"
      values$anchorText <- "F1"
      values$anchorLineLty <- 1
      values$addMissingOTUAfter <- NA
      values$missOTUspacings <- 0
      values$anchorTextMParental <- NA
      values$anchor <- T
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. macrophylla"),
        "and current representative of its main ancestors (Curk et al 2016)."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("DaCostaSilva2015", "Yamamoto_2007", "Curk_2016"))]
      values$legendYcoord <- 5
      values$groupName <- F
      values$spNumber <- 3
      values$refNumber <- 3
    } else if (input$karyotypeInput == "Citrus bergamia / A006-Bergamot") {
      values$dfChrSize <- objectList$threeBergamiaSize
      values$dfMarkPos <- objectList$threeBergamiaMarkPos
      values$dfMarkColor <- objectList$markStylethreeBergamiaDF

      values$leftNotes <- objectList$leftNotesBergamiadf
      values$notes <- objectList$notesBergamiadf
      values$moveKarHor <- "C. bergamia"
      values$anchorText <- "F1"
      values$anchorLineLty <- 1
      values$anchor <- T
      values$addMissingOTUAfter <- NA
      values$anchorTextMParental <- ""
      values$groupName <- T

      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. bergamia"),
        "and current representative of its main ancestors (Shimizu et al 2016).
                                  Hom: Homeologous chr. as determined by cited authors"
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Guerra2020", "Carvalho2005", "Shimizu2016"))]
      values$legendYcoord <- 5
      values$spNumber <- 3
      values$refNumber <- 3
    } else if (input$karyotypeInput %in% "Citrus komikan / A077(A076) Komikan") {
      values$dfChrSize <- objectList$threeKomikanSize
      values$dfMarkPos <- objectList$threeKomikanMarkPos
      values$dfMarkColor <- objectList$markStylethreeKomikanDF
      values$leftNotes <- objectList$leftNotesKomikandf
      values$notes <- objectList$notesKomikandf

      values$anchorText <- "F1"
      values$anchorLineLty <- 1

      values$moveKarHor <- "C. komikan"
      values$anchorTextMParental <- "Citrus leiocarpa hybrid"
      values$addMissingOTUAfter <- "C. komikan"
      values$missOTUspacings <- 1

      values$anchor <- T
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. komikan"),
        "and current representative of its main ancestor (Shimizu et al. 2016)."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Shimizu2016", "Yamamoto2003"))]
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 2.5
      values$refNumber <- 2
    } else if (input$karyotypeInput %in% "Citrus obovoidea / A055 Kinkoji") {
      values$dfChrSize <- objectList$threeObovoideaSize
      values$dfMarkPos <- objectList$threeObovoideaMarkPos
      values$dfMarkColor <- objectList$markStylethreeObovoideaDF
      values$leftNotes <- objectList$leftNotesObovoideadf
      values$notes <- objectList$notesObovoideadf

      values$anchorText <- "F1"
      values$anchorLineLty <- 1

      values$moveKarHor <- "C. obovoidea"
      values$anchorTextMParental <- "Citrus maxima hybrid"
      values$addMissingOTUAfter <- "C. obovoidea"
      values$missOTUspacings <- 1

      values$anchor <- T
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. obovoidea"),
        "and current representative of its main ancestor (Shimizu et al. 2016)."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Shimizu2016", "Cornelio2003"))]
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 2.5
      values$refNumber <- 2
    } else if (input$karyotypeInput %in% "Citrus taiwanica / A094 (A030) Nansho daidai") {
      values$dfChrSize <- objectList$threeTaiwanicaSize
      values$dfMarkPos <- objectList$threeTaiwanicaMarkPos
      values$dfMarkColor <- objectList$markStylethreeTaiwanicaDF
      values$leftNotes <- objectList$leftNotesTaiwanicadf
      values$notes <- objectList$notesTaiwanicadf

      values$anchorText <- "F1"
      values$anchorLineLty <- 1

      values$moveKarHor <- "C. taiwanica"
      values$anchorTextMParental <- NA
      values$addMissingOTUAfter <- "C. taiwanica"
      values$missOTUspacings <- 0

      values$anchor <- T
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. taiwanica"),
        "and current representative of its main ancestors (Shimizu et al. 2016)."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Shimizu2016", "Cornelio2003", "Yamamoto2005"))]
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 2.5
      values$refNumber <- 3
    } else if (input$karyotypeInput %in% "Citrus tamurana / A036 Hyuganatsu") {
      values$dfChrSize <- objectList$threeTamuranaSize
      values$dfMarkPos <- objectList$threeTamuranaMarkPos
      values$dfMarkColor <- objectList$markStylethreeTamuranaDF
      values$leftNotes <- objectList$leftNotesTamuranadf
      values$notes <- objectList$notesTamuranadf

      values$anchorText <- "F1"
      values$anchorLineLty <- 1

      values$moveKarHor <- "C. tamurana"
      values$anchorTextMParental <- "unknown"
      values$addMissingOTUAfter <- "C. tamurana"
      values$missOTUspacings <- 1

      values$anchor <- T
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. tamurana"),
        "and current representative of its main ancestor (Shimizu et al. 2016)."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Shimizu2016", "Yamamoto2003"))]
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 2.5
      values$refNumber <- 2
    } else if (input$karyotypeInput %in% "Citrus ujukitsu / A197 Ujukitsu") {
      values$dfChrSize <- objectList$threeUjukitsuSize
      values$dfMarkPos <- objectList$threeUjukitsuMarkPos
      values$dfMarkColor <- objectList$markStylethreeUjukitsuDF
      values$leftNotes <- objectList$leftNotesUjukitsudf
      values$notes <- objectList$notesUjukitsudf

      values$anchorText <- "F1"
      values$anchorLineLty <- 1

      values$moveKarHor <- "C. ujukitsu"
      values$anchorTextMParental <- "C. maxima hybrid"
      values$addMissingOTUAfter <- "C. ujukitsu"
      values$missOTUspacings <- 1

      values$anchor <- T
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. ujukitsu"),
        "and current representative of its main ancestor (Shimizu et al. 2016)."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Shimizu2016", "Befu2002"))]
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 2.5
      values$refNumber <- 2
    } else if (input$karyotypeInput %in% "curr. rep. true species") {
      values$dfChrSize <- objectList$threeTrueSpSize
      values$dfMarkPos <- objectList$threeTrueSpMarkPos
      values$dfMarkColor <- objectList$markStylethreeTrueSpDF
      values$leftNotes <- objectList$leftNotesTrueSpdf
      values$notes <- objectList$notesTrueSpdf

      values$anchorText <- "F1"
      values$anchorLineLty <- 1

      values$moveKarHor <- NA
      values$anchorTextMParental <- NA # "C. maxima hybrid"
      values$addMissingOTUAfter <- NA # "C. TrueSp"
      values$missOTUspacings <- NA # 1

      values$anchor <- F
      values$orderChr <- "chrNameUp"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        "current representative of",
        em("Citrus"),
        "true species."
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Guerra2020", "DaCostaSilva2015", "Yamamoto_2007"))]
      values$legendYcoord <- 7
      values$groupName <- F
      values$spNumber <- 4
      values$refNumber <- 3
    } else if (input$karyotypeInput == "Citrus limon / A085 Lemon") {
      values$dfChrSize <- objectList$threeLimonSize
      values$dfMarkPos <- objectList$threeLimonMarkPos
      values$dfMarkColor <- objectList$markStylethreeLimonDF
      values$leftNotes <- objectList$leftNotesLimondf
      values$notes <- objectList$notesLimondf

      values$anchorText <- "BC2"
      values$anchorLineLty <- 2

      values$moveKarHor <- "C. limon"
      values$anchorTextMParental <- NA # "C. maxima hybrid"
      values$addMissingOTUAfter <- NA # "C. ujukitsu"
      values$missOTUspacings <- 0

      values$anchor <- T
      values$orderChr <- "chrNameUp"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. limon"),
        "and current representative of its main ancestor (Shimizu et al. 2016).
                                  Hom: Homeologous chr. as determined by cited authors"
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Shimizu2016", "Carvalho2005", "Guerra2020", "DaCostaSilva2015"))]
      values$legendYcoord <- 0
      values$groupName <- F
      values$spNumber <- 3
      values$refNumber <- 4
    } else if (input$karyotypeInput == "Citrus aurantium / A141 Sour orange") {
      values$dfChrSize <- objectList$threeAurantiumSize
      values$dfMarkPos <- objectList$threeAurantiumMarkPos
      values$dfMarkColor <- objectList$markStylethreeAurantiumDF
      values$leftNotes <- objectList$leftNotesAurantiumdf
      values$notes <- objectList$notesAurantiumdf

      values$anchorText <- "F1"
      values$anchorLineLty <- 1

      values$moveKarHor <- "C. aurantium 'common'"
      values$anchorTextMParental <- "F1 pummelo × mandarin"
      values$addMissingOTUAfter <- NA # "C. aurantium 'common'"
      values$missOTUspacings <- 0

      values$anchor <- T
      values$orderChr <- "original"
      values$legend <- paste(
        "Figure. 2n = 18 Karyotypes of",
        em("C. aurantium"),
        "and current representative of its main ancestor (Shimizu et al. 2016).
                                  Hom: Homeologous chr. as determined by cited authors"
      )
      values$references <- refStrings[which(names(refStrings) %in% c("Shimizu2016", "Carvalho2005", "Guerra2020"))]
      values$legendYcoord <- 0
      values$groupName <- T
      values$spNumber <- 2.5
      values$refNumber <- 3
    }
  })

  observeEvent(input$karyotypeInput, {
    req(input$karyotypeInput)
    values$vertexNamesdf$selected <- "white"
    tryCatch(values$vertexNamesdf[which(values$vertexNamesdf$compo %in% input$karyotypeInput), ]$selected <- "selected",
      error = function(e) {
        "invalid selection"
      }
    )
  })

  myggnNodesSegments2 <- reactive({
    ggplot() +
      geom_point(
        data = objectList$vertexNamesdf,
        aes(
          x = x,
          y = y,
          shape = karyo,
          fill = karyo,
          color = karyo
        ),
        cex = 4
      ) +
      geom_point(
        data = objectList$singlesPos2,
        aes(
          x = x,
          y = y,
          fill = marker,
          color = marker
        ),
        shape = 22,
        cex = 4
      ) +
      geom_segment(
        aes(
          x = xorig,
          y = yorig,
          xend = xtarg2,
          yend = ytarg2,
          linetype = kinship
        ),
        data = objectList$networkdata
      ) +
      geom_segment(
        aes(
          x = xorigS, y = yorigS, xend = xtargS,
          yend = ytargS,
          linetype = kinship
        ),
        data = objectList$networkdata2
      ) +
      scale_linetype_manual(values = c("twodash", "solid")) +
      scale_color_manual(
        values = objectList$shapesSq3, labels = c("Karyo. (this work)"),
        breaks = "Karyotype (tw)",
        name = NULL
      ) +
      scale_shape_manual(
        values = objectList$myShapes,
        labels = c("Karyo. (this work)"),
        breaks = "Karyotype (tw)",
        name = NULL
      ) +
      scale_fill_manual(
        values = objectList$shapesSq3,
        labels = c("Karyo. (this work)"),
        breaks = "Karyotype (tw)",
        name = NULL
      ) +
      geom_nodelabel_repel(
        data = values$vertexNamesdf,
        aes(
          x = x, y = y,
          label = value,
          fill = selected,
          color = karyo,
          fontface = fontfaceK
        ),
        hjust = "inward",
        vjust = "inward",
        size = 4
      ) +
      guides(
        linetype = "none",
        fill = "none",
        shape = "none",
        color = "none"
      ) +
      theme_blank(base_size = 15)
  })

  #
  #   Shimizu version
  #

  myggnNodesSegmentsShi2 <- reactive({
    ggplot() +
      geom_point(
        data = objectList$vertexNamesdf,
        aes(
          x = x,
          y = y,
          shape = karyo,
          fill = karyo,
          color = karyo
        ),
        cex = 4
      ) +
      geom_point(
        data = objectList$singlesPos2,
        aes(
          x = x,
          y = y,
          fill = marker,
          color = marker
        ),
        shape = 22,
        cex = 4
      ) +
      geom_segment(
        aes(
          x = xorig,
          y = yorig,
          xend = xtarg2,
          yend = ytarg2,
          linetype = kinship
        ),
        data = objectList$networkdata
      ) +
      geom_segment(
        aes(
          x = xorigS, y = yorigS, xend = xtargS,
          yend = ytargS,
          linetype = kinship
        ),
        data = objectList$networkdata2
      ) +
      scale_linetype_manual(values = c("twodash", "solid")) +
      scale_color_manual(
        values = objectList$shapesSq3,
        labels = c("Karyo. (this work)"),
        breaks = "Karyotype (tw)",
        name = NULL
      ) +
      scale_shape_manual(
        values = objectList$myShapes,
        labels = c("Karyo. (this work)"),
        breaks = "Karyotype (tw)",
        name = NULL
      ) +
      scale_fill_manual(
        values = objectList$shapesSq3,
        labels = c("Karyo. (this work)")
        # ,
        , breaks = "Karyotype (tw)",
        name = NULL
      ) +
      geom_nodelabel_repel(
        data = values$vertexNamesdf,
        aes(
          x = x, y = y,
          label = valueShi,
          fill = selected,
          color = karyo,
          fontface = fontfaceM
        ),
        hjust = "inward",
        vjust = "inward",
        size = 4
      ) +
      guides(
        linetype = "none",
        fill = "none",
        shape = "none",
        color = "none"
      ) +
      theme_blank(base_size = 15)
  })

  myggnNodesSegmentsk3 <- reactive({
    myggnNodesSegments2() +
      lapply(
        objectList$dfk, function(z) {
          geom_polygon(z,
            mapping = aes(
              x = x,
              y = y,
              fill = id,
              group = id
            )
          )
        }
      ) +
      scale_fill_manual(values = objectList$shapeAndColor2)
  })


  myggnNodesSegmentsk4 <- reactive({
    myggnNodesSegments2() +
      lapply(
        objectList$dfk4, function(z) {
          geom_polygon(z,
            mapping = aes(
              x = x,
              y = y,
              fill = id,
              group = id
            )
          )
        }
      ) +
      scale_fill_manual(values = objectList$shapeAndColorPa2)
  })

  myggnNodesSegmentsk3Shi <- reactive({
    myggnNodesSegmentsShi2() +
      lapply(
        objectList$dfk, function(z) {
          geom_polygon(z,
            mapping = aes(
              x = x,
              y = y,
              fill = id,
              group = id
            )
          )
        }
      ) +
      scale_fill_manual(values = objectList$shapeAndColor2)
  })

  myggnNodesSegmentsk4Shi <- reactive({
    myggnNodesSegmentsShi2() +
      lapply(
        objectList$dfk4, function(z) {
          geom_polygon(z,
            mapping = aes(
              x = x,
              y = y,
              fill = id,
              group = id
            )
          )
        }
      ) +
      scale_fill_manual(values = objectList$shapeAndColorPa2)
  })


  myggnk4 <- reactive({
    gbuild <- ggplot_build(myggnNodesSegmentsk4())
    gtgbuild <- ggplot_gtable(gbuild)
    gtgbuild$layout$clip[gtgbuild$layout$name == "panel"] <- "off"
    myggnk4 <- as_ggplot(gtgbuild)
  })

  myggnk4Shi <- reactive({
    gbuild <- ggplot_build(myggnNodesSegmentsk4Shi())
    gtgbuild <- ggplot_gtable(gbuild)
    gtgbuild$layout$clip[gtgbuild$layout$name == "panel"] <- "off"
    myggnk4Shi <- as_ggplot(gtgbuild)
  })

  myggnk3 <- reactive({
    gbuild <- ggplot_build(myggnNodesSegmentsk3())
    gtgbuild <- ggplot_gtable(gbuild)
    gtgbuild$layout$clip[gtgbuild$layout$name == "panel"] <- "off"
    myggnk3 <- as_ggplot(gtgbuild)
  })

  myggnk3Shi <- reactive({
    gbuild <- ggplot_build(myggnNodesSegmentsk3Shi())
    gtgbuild <- ggplot_gtable(gbuild)
    gtgbuild$layout$clip[gtgbuild$layout$name == "panel"] <- "off"
    myggnk3Shi <- as_ggplot(gtgbuild)
  })

  output$k4plot <- renderImage(deleteFile = F, {
    outfile1 <- tempfile(fileext = ".svg")

    svg(outfile1, width = plotwidth, height = plotheight)

    if (input$checkboxk4 == FALSE) {
      grid.arrange(myggnk4())
    } else {
      grid.arrange(myggnk4Shi())
    }
    dev.off()

    list(
      src = normalizePath(outfile1),
      contentType = "image/svg+xml",
      width = width,
      height = height,
      alt = "karyotype"
    )
  })

  output$k3plot <- renderImage(deleteFile = F, {
    outfile2 <- tempfile(fileext = ".svg")
    svg(outfile2, width = plotwidth, height = plotheight)

    if (input$checkboxk3 == FALSE) {
      grid.arrange(myggnk3())
    } else {
      grid.arrange(myggnk3Shi())
    }
    dev.off()

    list(
      src = normalizePath(outfile2),
      contentType = "image/svg+xml",
      width = width,
      height = height,
      alt = "karyotype"
    )
  })

  output$karyoPlot <- renderImage(deleteFile = F, {
    outfile <- tempfile(fileext = ".svg")
    svg(outfile, width = plotwidth, height = values$spNumber * 3)
    par(mar = rep(0, 4), oma = rep(0, 4))
    # browser()
    if (is.na(values$moveKarHor)) values$moveKarHor <- ""
    plotIdiograms(
      dfChrSize = values$dfChrSize,
      dfMarkPos = values$dfMarkPos,
      dfMarkColor = values$dfMarkColor,
      orderChr = values$orderChr,
      chrIdPatternRem = "_.*",
      karHeight = 3,
      karHeiSpace = 8.5,
      chromatids = F,
      chrSpacing = .50,
      chrWidth = .5,
      chrColor = "gray",
      distTextChr = 1.2,
      legendWidth = 1.2,
      legendHeight = 3,
      markLabelSpacer = 2,
      legendYcoord = values$legendYcoord,
      bannedMarkName = "constric",
      ruler = FALSE,
      chrIndex = "",
      morpho = "",
      karIndex = FALSE,
      classChrName = "Type",
      groupName = values$groupName,
      classGroupName = "Hom",
      nameChrIndexPos = 5,
      lwd.chr = 1,
      addOTUName = F,
      OTUfont = 3,
      leftNotes = values$leftNotes,
      OTUasLeftNote = T,
      leftNoteFontUp = 3,
      notes = values$notes,
      notesTextSize = 1.3,
      leftNotesPosX = 0,
      leftNotesPosY = -0.5,
      leftNotesUpPosY = 2.5,
      ylimBotMod = 2,
      ylimTopMod = 2,
      xlimLeftMod = 2,
      xlimRightMod = 5,
      threshold = 50,
      moveKarHor = values$moveKarHor,
      mkhValue = 2,
      anchor = values$anchor,
      karSepar = F,
      chrNameUp = T,
      classChrNameUp = "Hom",
      xModMonoHoloRate = 2,
      anchorText = values$anchorText,
      anchorLineLty = values$anchorLineLty,
      addMissingOTUAfter = NA,
      missOTUspacings = 0,
      anchorTextMParental = values$anchorTextMParental,
      anchorTextMoveParenX = -.5,
      anchorTextMoveParenY = 1,
      OTUTextSize = 1.3
    )
    dev.off()

    list(
      src = normalizePath(outfile),
      contentType = "image/svg+xml",
      width = plotwidth * divisor,
      height = values$spNumber * 3 * divisor,
      alt = "karyotype"
    )
  })

  # Plot for Sankey Data table
  output$genealogyText <- renderText({
    "'The allele-sharing test inferred that bergamot arose from hybridization of lemon and sour orange as previously demonstrated [48]'; STRUCTURE K = 3 Bergamot 0.3430-pummelo 0.6319-citron"
  })

  output$authorText <- renderUI(HTML({
    paste(
      "<br></br>",
      "Shimizu et al. (2016)"
    )
  }))

  #     "Fig S2. Parental network and genome composition (admixture).
  # Parentals (circles) were deduced by Shimizu et al. (2016), except 
  # for C. macrophylla (Curk et al. 2016), red ones established by cytoplasm genotypes, 
  # gray ones by admixture. Most admixture bars were determined by Shimizu et al. (2016), 
  # except for C. macrophylla (Curk et al., 2016), C. webberi and C. longispina (Ramadugu et al., 2013).
  #  Colored squares in C. hystrix and C. iwaikan indicate proof of genetic relationship to ancestral 
  # species by Nicolosi et al (2007) and Wu et al. (2014), respectively."
  #


  legendTextPlain <- paste(
    "Fig S2. Parental network and genome composition (admixture - change k above). Parentals (circles) were deduced by Shimizu et al. (2016), except for ",
    em("C. macrophylla"),
    "(Curk et al. 2016), red ones established by cytoplasm genotypes, gray ones by admixture. Most admixture % (bars) were determined by Shimizu et al. (2016), except for",
    em("C. macrophylla"),
    "(Curk et al., 2016),",
    em("C. webberi"), ", and",
    em("C. longispina"),
    "(Ramadugu et al., 2013). Colored squares in", em("C. hystrix"), " and ",
    em("C. iwaikan"),
    "indicate proof of genetic relationship to ancestral species found by Nicolosi et al (2007) and Wu et al. (2014), respectively."
  ) # p

  output$k4Text <- renderUI(HTML(
    legendTextPlain
  ))

  output$k3Text <- renderUI(HTML(
    legendTextPlain
  ))

  output$mytabBox <- renderUI(
    wellPanel(
      style = "overflow-y:scroll; max-height: 1450px",
      h4("Karyotypes"),
      uiOutput("karyoUI"),
      div(
        style = "overflow-y:auto;overflow-x:auto",
        uiOutput("karyoUI2")
      ),
      uiOutput("karyoUI3")
    )
  )
}
