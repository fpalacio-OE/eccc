verthortieup <- function(seriestable, tagtable, gvatag) {
  gvaseries2 <- seriestable


  for (i in 1:5) {
    # reshape series
    gvaseries2 <- dcast(gvaseries2, code + year + variable ~ geography)
    gvaseries2 <- gvaseries2[order(variable, code, year)]

    # collect fixes
    tagsformerge <- tagtable

    # move Canada to the end
    setcolorder(gvaseries2, c(setdiff(names(gvaseries2), "Canada"), "Canada"))

    # bring in canada fixes
    canadatags <- tagtable[geography == "Canada", .(code, year, variable, tag)]
    gvaseries2 <- canadatags[gvaseries2, on = c("variable", "code", "year")]

    # calculate row sums
    notcanada <- names(gvaseries2)[!(names(gvaseries2) %in% c("Canada", "year", "code", "variable", "tag"))]
    gvaseries2[, provtotal := rowSums(.SD), .SDcols = (notcanada)]
    gvaseries2[tag == 1, scale := (Canada - provtotal) / provtotal]
    gvaseries2[tag != 1, Canada := rowSums(.SD), .SDcols = (notcanada)]
    # split apart and merge for fixes
    scales <- gvaseries2[, c("code", "year", "scale", "variable"), with = F]
    nationalvals <- melt(gvaseries2[, c("code", "year", "variable", "Canada"), with = F], id.vars = c("code", "year", "variable"), variable.name = "geography")
    if (gvatag == 1) {
      nationalvals <- nationalvals[variable == "gva - current prices (x 1,000,000)"]
    }

    gvaseriesremelt <- melt(gvaseries2[, c("code", "year", "variable", eval(notcanada)), with = F], id.vars = c("code", "year", "variable"), variable.name = "geography")

    # db ready:
    gvaseriesforhorvert <- scales[gvaseriesremelt, on = c("code", "year", "variable")]
    if (gvatag == 1) {
      gvaseriesforhorvert <- gvaseriesforhorvert[variable == "gva - current prices (x 1,000,000)"]
    }

    # quick transform
    gvaseriesforhorvert[, level := shiftshares$level[match(code, shiftshares$code, nomatch = NA)]]

    gvaseriesforhorvert[level == i & abs(scale) > .05, tag := 1]
    gvaseriesforhorvert[tag == 1, value := value + scale * value]

    # bring back old fixes and umbrella
    tagsformerge <- tagtable
    gvaseriesforhorvert <- tagsformerge[gvaseriesforhorvert, on = c("geography", "variable", "code", "year")]
    gvaseriesforhorvert[, tag := ifelse(is.na(i.tag), tag, i.tag)][, c("level", "i.tag", "scale") := NULL]
    gvaseriesforhorvert[, tag := ifelse(is.na(tag), 0, tag)]
    gvaseriesforhorvert[, umbrella := shiftshares$umbrella[match(gvaseriesforhorvert$code, shiftshares$code, nomatch = NA)]]

    # This function is used again and works as long as it has this colname structure : "geography" "code"  "umbrella"    "year"      "variable"  "value" "tag"
    gvaseries2 <- tieupfunction(gvaseriesforhorvert)
    suppressWarnings(gvaseries2 <- rbind(gvaseries2[, !c("umbrella", "tag"), with = F], nationalvals))

    compares <- merge(gvaseries2, gvaseriesremelt, by = c("geography", "variable", "code", "year"))
    compares <- merge(compares, tagtable, by = c("geography", "variable", "code", "year"))
    setnames(compares, c("value.x", "value.y"), c("new", "old"))
    # fix if values seem to converge, or if it's real data
    compares[, level := shiftshares$level[match(code, shiftshares$code, nomatch = NA)]]
    compares[level == (i + 1) & !is.na(new) & !is.na(old), tag2 := (ifelse(abs(new - old) < .05, 1L, 0L))]
    compares[level == (i + 1) & !is.na(new) & !is.na(old), tag := as.numeric(ifelse(tag == 1, as.numeric(1), as.numeric(tag2)))]

    tagtable <- compares[, c("new", "old", "level", "tag2") := NULL]
  }

  gvaseriesforhorvert <- gvaseries2 # [geography!="Canada"]


  # bring back national values and chained values
  # gvahorfixed<- rbind(nationalvals,gvaseriesforhorvert[, c("umbrella", "sectorsum","tag"):=NULL])
  if (gvatag == 1) {
    suppressWarnings(gvachained <- forgrowbackcomplete[, c("umbrella", "tag") := NULL])
    gvahorfixed <- rbind(gvaseriesforhorvert, gvachained[variable == "gva - chained prices (x 1,000,000)"])
    #print(nrow(gvahorfixed))
    gvahorfixed[, variable := as.character(variable)]
  } else {
    gvahorfixed <- gvaseriesforhorvert[, variable := as.character(variable)]
  }
}
