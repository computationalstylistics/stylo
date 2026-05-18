
config_write = function(params, specs, file = "") {

  cat("", file = file)

  for (section in specs) {

    cat(
      paste0(
        "\n#################################################\n",
        "# ", section$title, "\n",
        "#################################################\n"
      ),
      file = file,
      append = TRUE
    )


    description = gsub("\n", "\n# ", section$description)
    description = gsub("# [\t ]+", "# ", description)
    cat(
      paste0("# ", description, "\n\n"),
      file = file,
      append = TRUE
    )

    for (field_name in names(section$fields)) {

      field = section$fields[[field_name]]

      # comments
      comment = gsub("\n", "\n# ", field$comment)
      comment = gsub("# [\t ]+", "# ", comment)
      cat(
        paste0("# ", comment, "\n"),
        file = file,
        append = TRUE
      )

      # allowed values
      if (!is.null(field$allowed)) {

        cat(
          paste0(
            "# Allowed values: ",
            paste(field$allowed, collapse = ", "),
            "\n"
          ),
          file = file,
          append = TRUE
        )
      }

      # actual value
      value = params[[field_name]]

      if (is.character(value)) {
        value_txt = paste0('"', value, '"')
      } else {
        value_txt = as.character(value)
      }

      cat(
        paste0(field_name, " = ", value_txt, "\n\n"),
        file = file,
        append = TRUE
      )
    }
  }
}

