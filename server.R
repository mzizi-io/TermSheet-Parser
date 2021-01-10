shinyServer(function(input, output, session){
observeEvent(input$send_email, {
  # convert mails into a single string
  selected_mails <- input$mail_list %>%
    unlist()%>%
    paste0(collapse = ", ")
  
  # Send the mail
  output$out_mail_list <- renderText({selected_mails})
  
  # Send success email if succeeded
  if(is.variable(selected_mails)){
        sendSweetAlert(session = session,
                          title = "Success !!",
                          text = "Mail successfully sent. Please check the outbox section of your mail",
                          type = "success")}
  }
)
  
})