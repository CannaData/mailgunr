#' Send Email with mailgun API
#' 
#' @export
#' @import httr jsonlite
#' 
mg_send_email <- function(from, to, cc = NULL, bcc = NULL, subject = NULL, 
                    text = NULL, html = NULL, attachment = NULL, 
                    inline = NULL, tag = NULL, campaign = NULL, 
                    dkim = NULL, deliverytime = NULL, testmode = NULL, 
                    tracking = NULL, tracking_clicks = NULL,
                    tracking_opens = NULL, required_tls = NULL, 
                    skip_verification = NULL, 
                    x_my_header = NULL, my_var = NULL,
                    mailgun_key = Sys.getenv("mailgun_key"),
                    mailgun_domain = Sys.getenv("mailgun_domain")) {
  
  base_url <- "https://api.mailgun.net/"
  
  path <- paste(sep = "/", "v3", mailgun_domain, "messages")
  
  url <- modify_url(base_url, path = path)
  
  if (!is.null(attachment)) {
    attachment_file <- upload_file(attachment)
  }
  
  resp <- POST(url, authenticate("api", mailgun_key),
       body = list(
         from = from, to = to, cc = cc, bcc = bcc, subject = subject, 
         text = text, html = html, attachment = attachment_file, 
         inline = inline, tag = tag, campaign = campaign, 
         dkim = dkim, deliverytime = deliverytime, testmode = testmode, 
         tracking = tracking, tracking_clicks = tracking_clicks,
         tracking_opens = tracking_opens, required_tls = required_tls, 
         skip_verification = skip_verification, 
         x_my_header = x_my_header, my_var = my_var
       ), encode = "multipart")
  
  if (http_type(resp) != "application/json") {
    stop("mailgun API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("mailgun API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  structure(
    parsed,
    class = "mailgun_result"
  )
  
}

#' @export
mg_get_mailing_lists <- function(mailgun_key = Sys.getenv("mailgun_key"),
                                 mailgun_domain = Sys.getenv("mailgun_domain")) {
  base_url <- "https://api.mailgun.net/"
  
  path <- paste(sep = "/", "v3", "lists")
  
  url <- modify_url(base_url, path = path)
  
  resp <- GET(url, authenticate("api", mailgun_key))
  
  if (http_type(resp) != "application/json") {
    stop("mailgun API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("mailgun API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  parsed
  
}

#' @export
mg_make_mailing_list <- function(address, name = NULL, description = NULL, access_level = NULL, mailgun_key = Sys.getenv("mailgun_key")) {
  base_url <- "https://api.mailgun.net/"
  
  path <- paste(sep = "/", "v3", "lists")
  
  url <- modify_url(base_url, path = path)
  
  resp <- POST(url, authenticate("api", mailgun_key),
               body = list(
                 address = address,
                 name = name,
                 description = description,
                 access_level = access_level
               ), encode = "multipart")
  
  if (http_type(resp) != "application/json") {
    stop("mailgun API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("mailgun API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  parsed
}

#' @export
mg_add_email_mailing_list <- function(mail_list_address, address, name = NULL, vars = NULL, 
                                      subscribed = NULL, upsert = NULL, mailgun_key = Sys.getenv("mailgun_key")) {
  base_url <- "https://api.mailgun.net/"
  
  path <- paste(sep = "/", "v3", "lists", mail_list_address, "members")
  
  url <- modify_url(base_url, path = path)
  
  resp <- POST(url, authenticate("api", mailgun_key),
               body = list(
                 address = address,
                 name = name,
                 vars = vars,
                 subscribed = subscribed,
                 upsert = upsert
               ), encode = "multipart")
  
  if (http_type(resp) != "application/json") {
    stop("mailgun API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("mailgun API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  parsed
}

#' @export
mg_edit_email_mailing_list <- function(mail_list_address, address, name = NULL, vars = NULL, subscribed = NULL, upsert = NULL, mailgun_key = Sys.getenv("mailgun_key")) {
  base_url <- "https://api.mailgun.net/"
  
  path <- paste(sep = "/", "v3", "lists", mail_list_address, "members", address)
  
  url <- modify_url(base_url, path = path)
  
  resp <- POST(url, authenticate("api", mailgun_key),
               body = list(
                 address = address,
                 name = name,
                 vars = vars,
                 subscribed = subscribed
               ), encode = "multipart")
  
  if (http_type(resp) != "application/json") {
    stop("mailgun API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("mailgun API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  parsed <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  parsed
}

