# serverAIWidget.R

server_ai_widget <- function(input, output, session, ai_context) {
  
  # Chat History State
  ai_chat_history <- reactiveVal(character(0))
  
  # Summary Result State
  ai_summary_res <- reactiveVal("")
  
  # Helper to format chat history as HTML
  output$ai_global_chat_history <- renderUI({
    hist <- ai_chat_history()
    if (length(hist) == 0) {
      return(HTML("<div style='color: #888; text-align: center; margin-top: 20px;'>Belum ada percakapan.</div>"))
    }
    HTML(paste(hist, collapse = "<br><br>"))
  })
  
  # Render Summary
  output$ai_global_summary_result <- renderUI({
    res <- ai_summary_res()
    if (res == "") {
      return(HTML("<div style='color: #888; text-align: center;'>Ringkasan hasil analisis akan muncul di sini.</div>"))
    }
    HTML(res)
  })
  
  # Chat Send Logic
  observeEvent(input$ai_global_chat_send, {
    prompt <- trimws(input$ai_global_chat_input)
    if (prompt == "") return()
    
    # Update input to empty
    updateTextInput(session, "ai_global_chat_input", value = "")
    
    # Add User message
    current_chat <- ai_chat_history()
    user_msg <- paste0("<b>You:</b><br>", htmltools::htmlEscape(prompt))
    current_chat <- c(current_chat, user_msg)
    ai_chat_history(current_chat)
    
    session$sendCustomMessage("scroll_ai_chat", "scroll")
    
    # We could show a thinking indicator or just append it
    current_chat <- c(current_chat, "<div id='ai_thinking' style='color:#666;'><i>AI is thinking...</i></div>")
    ai_chat_history(current_chat)
    session$sendCustomMessage("scroll_ai_chat", "scroll")
    
    # Construct full prompt with context
    ctx_text <- ai_context$results_text
    sys_prompt <- "You are an AI assistant for measureR. You are in a conversational Q&A mode. The user's latest analysis results are provided as Context. ONLY answer the user's specific Question. DO NOT generate a full summary of the Context unless explicitly asked by the user in the Question."
    full_prompt <- paste0(sys_prompt, "\n\nContext:\n", ctx_text, "\n\nQuestion:\n", prompt)
    
    # Call AI
    tryCatch({
      response <- ask_ai(full_prompt, input$ai_provider, input$ai_model, input$ai_api_key)
      response_html <- gsub("\n", "<br>", response)
      
      # Remove thinking and add response
      current_chat <- current_chat[-length(current_chat)]
      ai_msg <- paste0("<b>AI:</b><br>", response_html)
      current_chat <- c(current_chat, ai_msg)
      ai_chat_history(current_chat)
      
    }, error = function(e) {
      current_chat <- current_chat[-length(current_chat)]
      err_msg <- paste0("<b>AI Error:</b><br><span style='color:red;'>", e$message, "</span>")
      current_chat <- c(current_chat, err_msg)
      ai_chat_history(current_chat)
    })
    
    session$sendCustomMessage("scroll_ai_chat", "scroll")
  })
  
  # Clear Chat
  observeEvent(input$ai_global_chat_clear, {
    ai_chat_history(character(0))
  })
  
  # Generate Summary Logic
  observeEvent(input$ai_global_summary_generate, {
    ctx_text <- ai_context$results_text
    
    if (is.null(ctx_text) || trimws(ctx_text) == "") {
      ai_summary_res("<span style='color:red;'>Tidak ada hasil analisis yang aktif. Jalankan sebuah analisis terlebih dahulu.</span>")
      return()
    }
    
    ai_summary_res("<i>Generating summary...</i>")
    
    format <- input$ai_summary_format
    lang <- input$ai_summary_lang
    
    # Parse context
    context_str <- ""
    if (!is.null(input$ai_context_text) && trimws(input$ai_context_text) != "") {
      context_str <- trimws(input$ai_context_text)
    }
    
    if (!is.null(input$ai_context_file)) {
      file_path <- input$ai_context_file$datapath
      ext <- tools::file_ext(input$ai_context_file$name)
      file_content <- tryCatch({
        if (tolower(ext) == "txt") {
          paste(readLines(file_path, warn = FALSE), collapse = "\n")
        } else if (tolower(ext) == "pdf" && requireNamespace("pdftools", quietly = TRUE)) {
          paste(pdftools::pdf_text(file_path), collapse = "\n")
        } else if (tolower(ext) == "docx" && requireNamespace("officer", quietly = TRUE)) {
          doc <- officer::read_docx(file_path)
          content <- officer::docx_summary(doc)
          paste(content$text[!is.na(content$text)], collapse = "\n")
        } else {
          ""
        }
      }, error = function(e) { paste("Error reading file:", e$message) })
      
      if (file_content != "") {
        context_str <- paste0(context_str, "\n\n[Konteks dari Dokumen File:]\n", file_content)
      }
    }
    
    context_prompt_part <- ""
    if (context_str != "") {
      context_prompt_part <- paste0("PERHATIKAN KONTEKS PENELITIAN BERIKUT (Gunakan untuk menyesuaikan interpretasi Anda):\n", context_str, "\n\n")
    }

    # Manuscript format specific instruction
    manuscript_instruction <- ""
    if (format == "Manuscript") {
      manuscript_instruction <- paste0(
        "Karena format yang diminta adalah 'Manuscript (Narrative)', Anda HARUS menulis laporan bergaya artikel jurnal. ",
        "Susun kalimat menjadi paragraf-paragraf yang utuh, mengalir, dan profesional. ",
        "Anda HARUS menyebutkan/merujuk ke tabel atau gambar di dalam teks (misalnya: 'Seperti yang disajikan pada Tabel 1...' atau 'Berdasarkan Gambar 1...'). ",
        "Anda HARUS menyisipkan spasi kosong/placeholder tepat setelah paragraf yang merujuk tabel/gambar tersebut dengan format `[Sisipkan Tabel/Gambar X di sini]`. Jangan membuat tabelnya, cukup tulis placeholdernya saja agar pengguna bisa menempelkan tabel asli dari aplikasi nanti.\n"
      )
    }
    
    prompt <- paste0(
      "Tugas Anda HANYA membuat ringkasan komprehensif dari hasil analisis berikut. ",
      "JANGAN tambahkan sapaan chat, basa-basi, atau teks percakapan apa pun.\n",
      context_prompt_part,
      "PENTING: Jika hasil analisis mencakup sejarah/riwayat beberapa model (seperti Model_1, Model_2, dst.), ",
      "pastikan Anda menceritakan bagaimana kondisi model awal, alasan/detail modifikasi yang dilakukan pada model-model selanjutnya, ",
      "serta bagaimana modifikasi tersebut memperbaiki hasil akhir (perbandingan model).\n",
      manuscript_instruction,
      "WAJIB: Selalu sertakan kutipan (in-text citation) untuk standar atau kriteria (misalnya Hu & Bentler, 1999; Hair et al., 2010; Schreiber et al., 2006, dll). ",
      "Selain itu, WAJIB tambahkan satu bagian khusus berjudul 'Referensi' di akhir ringkasan Anda yang memuat daftar pustaka lengkap dari semua sitasi yang Anda gunakan, sehingga referensi ini ikut terekspor.\n\n",
      "Format yang diminta: ", format, "\n",
      "Gunakan bahasa: ", lang, "\n\n",
      "Hasil Analisis:\n", ctx_text
    )
    
    tryCatch({
      response <- ask_ai(prompt, input$ai_provider, input$ai_model, input$ai_api_key)
      response_html <- gsub("\n", "<br>", response)
      ai_summary_res(response_html)
    }, error = function(e) {
      ai_summary_res(paste0("<span style='color:red;'>Error: ", e$message, "</span>"))
    })
  })

  # Add to report
  observeEvent(input$ai_add_to_report, {
    res <- ai_summary_res()
    if (res != "" && !grepl("<i>Generating summary...</i>", res)) {
      ai_context$ai_report_text <- res
      showNotification("AI Summary added! It will be included when you export HTML report.", type = "message", duration = 5)
    } else {
      showNotification("Please generate an AI summary first.", type = "warning")
    }
  })

  
  # Save API Key from Widget
  observeEvent(input$save_ai_widget_api_key_chat, {
    key <- trimws(input$ai_widget_api_key_chat)
    if (key != "") {
      updateSelectInput(session, "ai_provider", selected = input$ai_widget_provider_chat)
      updateTextInput(session, "ai_model", value = input$ai_widget_model_chat)
      updateTextInput(session, "ai_api_key", value = key)
    }
  })
  
  observeEvent(input$save_ai_widget_api_key_sum, {
    key <- trimws(input$ai_widget_api_key_sum)
    if (key != "") {
      updateSelectInput(session, "ai_provider", selected = input$ai_widget_provider_sum)
      updateTextInput(session, "ai_model", value = input$ai_widget_model_sum)
      updateTextInput(session, "ai_api_key", value = key)
    }
  })
}
