#' @title get_ntp_dup_keys 
#' @description Helper function to return a dataframe to rekey duplicate entries
#' @param df Input list of extracted CvT data sheets
#' @param sheet Name of the sheet to find dup keys
#' @return Dataframe of duplicate NTP keys with new key to assign.
#' @seealso 
#'  [group_by][dplyr::group_by], [across][dplyr::across], [mutate][dplyr::mutate], [ungroup][dplyr::ungroup], [select][dplyr::select], [distinct][dplyr::distinct], [filter][dplyr::filter], [everything][dplyr::everything], [arrange][dplyr::arrange]
#'  [separate][tidyr::separate], [separate_rows][tidyr::separate_rows]
#' @rdname get_ntp_dup_keys
#' @export 
#' @importFrom dplyr group_by across mutate ungroup select distinct filter everything arrange
#' @importFrom tidyr separate separate_rows
get_ntp_dup_keys <- function(df, sheet){
  orig_ids <- data.frame(id_new = df[[sheet]]$id,
                         dup_id = df[[sheet]]$id)
  # Get initial list of dups
  dup_list <- df[[sheet]] %>%
    dplyr::group_by(dplyr::across(c(-id))) %>%
    # Get "," separated list of related keys
    dplyr::mutate(id_groups = paste0(id, collapse=", ")) %>%
    dplyr::ungroup() %>%
    # filter(grepl(", ", id_groups)) %>%
    dplyr::select(id_groups) %>%
    dplyr::distinct() %>%
    # Determine the parent key
    tidyr::separate(id_groups, into=c("id_new", "dup_id"), sep=", ", 
                    extra="merge", fill = "right")
  # Fill in any NA values (parent but no dups)
  dup_list$dup_id[is.na(dup_list$dup_id)] <- dup_list$id_new[is.na(dup_list$dup_id)]
  # Split out the list into a map
  dup_list <- dup_list %>%
    tidyr::separate_rows(dup_id, sep=", ")
  # Fill in keys that do not need to be changed
  dup_list <- dup_list %>%
    rbind(orig_ids %>% 
            dplyr::filter(!id_new %in% dup_list$dup_id)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~as.numeric(.))) %>%
    # Sort and arrange fields before return
    dplyr::arrange(dup_id, id_new) %>%
    dplyr::select(dup_id, id_new) %>%
    return()
}
