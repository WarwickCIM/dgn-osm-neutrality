#' Get OSM stats
#'
#' @param url the url to get the stats from. Defaults to https://planet.openstreetmap.org/statistics/data_stats.html
#' @param df_type a string with either "wide" or "long". Defines whether the output will be a long or wide dataframe.
#'
#' @returns a data frame in wide (or long) format containing OSM stats.
#' @export
#'
#' @examples
get_osm_stats <- function(
      url = "https://planet.openstreetmap.org/statistics/data_stats.html",
      df_type = "wide"
    ){
  
  osm_stats <- rvest::read_html(url) |> 
    html_node("table") |> 
    html_table()
  
  names(osm_stats)[names(osm_stats) == "X1"] <- "metric"
  names(osm_stats)[names(osm_stats) == "X2"] <- "value"
  
  if(df_type == "long") {
    osm_stats <- osm_stats |> 
      tidyr::pivot_wider(names_from = metric, values_from = value)
  }
  
  return(osm_stats)
}



#' Gets users from OSM's Wiki
#'
#' @param api_url 
#' @param file_path A string containing the file path to save the data to.
#'
#' @returns a dataframe with all names and ids for every user in the wiki.
#' @export
#'
#' @examples
get_users_wiki <- function(
      file_path = NULL,
      api_url = "https://wiki.openstreetmap.org/w/api.php"
    ) {
  
  # Initialize an empty data frame to store user information
  all_users <- data.frame()
  
  # Set the initial 'aufrom' parameter to an empty string
  aufrom <- ""
  
  repeat {
    # Define the query parameters
    params <- list(
      action = "query",
      list = "allusers",
      aufrom = aufrom,
      format = "json",
      aulimit = "max"
    )
    
    # Make the API request
    response <- GET(api_url, query = params)
    
    # Parse the response
    content <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Extract user information
    users <- content$query$allusers
    
    # Append the users to the data frame
    all_users <- rbind(all_users, users)
    
    # Check if there is a 'continue' parameter
    if (!is.null(content$continue)) {
      aufrom <- content$continue$aufrom
    } else {
      break
    }
  }
  
  if(details == TRUE) {
    # TODO: visit every user page and retrieve all the templates that they are
    # using to populate a wide dataframe.
    print("Still not implemented")
    
    # user_registration <- user_information(user_names= "Ccamara", domain = "wiki.openstreetmap.org", properties = "registration")
    # user_registration <- as.data.frame(user_registration$query$users)
  }
  
  if(!is.null(file_path)) {
    write.csv(all_users, file = file_path, row.names = FALSE)
  }
  
  return(all_users)
}

  
#' Retrieves pages and categories within a category page
#'
#' @param url 
#'
#' @returns a dataframe with the number of pages and categories for every variable in a given page.
#' @export
#'
#' @examples
get_users_category <- function(
    url = "https://wiki.openstreetmap.org/wiki/Category:Users_by_country",
    debug = FALSE
  ){
  
  wiki_scrape <-  rvest::read_html(url) |> 
    html_elements("li") |> 
    html_text2()
  
  df <- as.data.frame(wiki_scrape) |> 
    dplyr::rename(content_raw = 1) |> 
    dplyr::filter(stringr::str_starts(content_raw, "Users in")) |> 
    dplyr::mutate(content = content_raw,
                  content = stringr::str_remove(content, "Users in "),
                  content = stringr::str_remove(content, "\\(country\\)")) |> 
    tidyr::separate_wider_delim(content, " (", names = c("variable", "value"), cols_remove = FALSE, too_few = "align_start", too_many ="debug") |> 
    # tidyr::separate(content, into = c("variable", "value"), sep = " ", extra = "merge") |> 
    dplyr::mutate(value = stringr::str_remove_all(value, "[()]")) |> 
    dplyr::mutate(categories = stringr::str_extract(value, "\\d+ C"),
                  pages = stringr::str_extract(value, "\\d+ P"),
                  files = stringr::str_extract(value, "\\d+ F")) |> 
    dplyr::mutate(categories = as.numeric(stringr::str_remove(categories, " C")),
                  pages = as.numeric(stringr::str_remove(pages, " P")),
                  files = as.numeric(stringr::str_remove(files, " F"))) 
  
  if(debug != TRUE) {
    df <- df |> 
      dplyr::select(variable, categories, pages, files)
  }
  
  return(df)
  
}

#' Retrieves pages and categories of a given language
#'
#' @param url 
#'
#' @returns a dataframe with the number of pages and categories for every variable in a given page.
#' @export
#'
#' @examples
get_users_language <- function(
    url = "https://wiki.openstreetmap.org/wiki/Category:Users_by_language",
    details = FALSE,
    debug = FALSE
){
  
  wiki_scrape <-  rvest::read_html(url) |> 
    html_elements("li") |> 
    html_text2()
  
  df <- as.data.frame(wiki_scrape) |> 
    dplyr::rename(content_raw = 1) |> 
    dplyr::filter(stringr::str_starts(content_raw, "User")) |> 
    dplyr::filter(content_raw != "Users") |> 
    dplyr::mutate(content = content_raw,
                  content = stringr::str_remove(content, "User "),
                  content = stringr::str_remove(content, "\\(country\\)")) |> 
    tidyr::separate_wider_delim(content, " (", names = c("variable", "value"), cols_remove = FALSE, too_few = "align_start") |> 
    # tidyr::separate(content, into = c("variable", "value"), sep = " ", extra = "merge") |> 
    dplyr::mutate(value = stringr::str_remove_all(value, "[()]")) |> 
    dplyr::mutate(categories = stringr::str_extract(value, "\\d+ C"),
                  pages = stringr::str_extract(value, "\\d+ P"),
                  files = stringr::str_extract(value, "\\d+ F")) |> 
    dplyr::mutate(categories = as.numeric(stringr::str_remove(categories, " C")),
                  pages = as.numeric(stringr::str_remove(pages, " P")),
                  files = as.numeric(stringr::str_remove(files, " F"))) 
  
  if(debug != TRUE) {
    df <- df |> 
      dplyr::select(variable, categories, pages, files)
  }
  
  # Get information about language level
  if(details == TRUE) {
    
    # Initialise empty dataframe
    df_levels <- data.frame()
    
    for(language in df$variable) {
      
      print(language)
      
      wiki_scrape <-  rvest::read_html(
        paste0("https://wiki.openstreetmap.org/wiki/Category:User_",
               language)
        ) |> 
        rvest::html_element("ul") |> 
        rvest::html_elements("li") |> 
        rvest:: html_text2()
      
      
      
      tmp_df_levels <- as.data.frame(wiki_scrape) |> 
        dplyr::rename(content_raw = 1) |> 
        dplyr::mutate(content = content_raw) |> 
        dplyr::mutate(content = stringr::str_remove(content, "User ")) |> 
        dplyr::mutate(content = stringr::str_remove(content, "‎"),
                      content = stringr::str_remove(content, "-BR"),
                      content = stringr::str_remove(content, "-[hH]ans*"),
                      content = stringr::str_remove(content, "-[hH]ant*")) |> 
        dplyr::filter(stringr::str_detect(content, "[[:alpha:]]+-[a-zA-Z0-9_] ")) |> 
        tidyr::separate_wider_delim(content, "-", names = c("language", "level_raw"), too_few = "align_start") |> 
        tidyr::separate_wider_delim(level_raw, " (", names = c("level", "value"), too_few = "align_start") |> 
        dplyr::mutate(value = stringr::str_remove_all(value, "[()]")) |> 
        dplyr::mutate(categories = stringr::str_extract(value, "\\d+ C"),
                      pages = stringr::str_extract(value, "\\d+ P"),
                      files = stringr::str_extract(value, "\\d+ F")) |> 
        dplyr::mutate(categories = as.numeric(stringr::str_remove(categories, " C")),
                      pages = as.numeric(stringr::str_remove(pages, " P")),
                      files = as.numeric(stringr::str_remove(files, " F"))) 
      
      
      # if(debug != TRUE) {
      #   tmp_df_levels <- tmp_df_levels |> 
      #     dplyr::select(variable, categories, pages, files)
      # }
    
      df_levels <- df_levels |> 
        dplyr::bind_rows(tmp_df_levels)
      
    }
    
    return(df_levels)
    
  } else {
    
    return(df)
  }
}
