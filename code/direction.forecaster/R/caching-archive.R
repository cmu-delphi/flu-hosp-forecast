
# Simple caching for now; not doing smart minimal updates. Relicensing code
# originally in cmu-delphi/epiforecast.






#' Fetch or read cache of a resource that potentially updates over time
#'
#' @param fetch a function that takes no arguments that, when called, attempts
#'   to fetch the desired resource and returns the response
#' @param check_response a function that takes a fetch response as input and
#'   either (a) stops if the response indicates an unsuccessful fetch or (b)
#'   returns nothing; this prevents invalid fetch responses from being saved to
#'   the cache.
#' @param cache_file_prefix length-1 \code{character} containing a filepath
#'   prefix to use when creating files to cache the fetch response, or
#'   \code{NULL} to disable caching
#' @param cache_invalidation_period length-1 \code{difftime}; the minimum amount
#'   of time that must pass between the last cache update and a call to this
#'   function for \code{fetch} to be called again rather than using the cache
#'   result (unless \code{force_cache_invalidation=TRUE})
#' @param force_cache_invalidation \code{TRUE} to force a cache update, even if
#'   the cache_invalidation_period has not passed; otherwise \code{FALSE}
#' @param silent \code{TRUE} or \code{FALSE} (default); \code{TRUE} to disable
#'   messages about whether the cache is being used or not, or \code{FALSE} to
#'   enable these messages
#' @param make_ancestor_directories Boolean; \code{TRUE} for any nonexistent
#'   ancestor directories of cache files to be automatically created, or
#'   \code{FALSE} for errors to be raised if these directories don't already
#'   exist
#'
#' @export
fetch_updating_resource = function(fetch,
                                   check_response,
                                   cache_file_prefix=NULL,
                                   cache_invalidation_period=as.difftime(1L, units="days"),
                                   force_cache_invalidation=FALSE,
                                   silent=FALSE,
                                   make_ancestor_directories=TRUE
                                   ) {
  cache_filepath =
    if (is.null(cache_file_prefix)) {
      NULL
    } else {
      paste0(cache_file_prefix, ".RDS")
    }
  should_fetch_now =
    if (is.null(cache_filepath)) {
      TRUE
    } else if (force_cache_invalidation) {
      TRUE
    } else if (!file.exists(cache_filepath)) {
      TRUE
    } else {
      fetch_info = readRDS(cache_filepath) # ==> fetch_info
      should_refetch = difftime(Sys.time(), fetch_info$fetch_timestamp) > cache_invalidation_period
      should_refetch
    }

  if (should_fetch_now) {
    if (!silent) {
      message("No cache, empty cache, expired cache, or forced cache invalidation; fetching data.")
    }
    # todo prompt for confirmation on fetch/refetch
    # todo option to read from cache without considering refetch
    #
    # `Sys.time()` outputs a POSIXct, which tracks the time elapsed since some
    # epoch, paired with timezone metadata. Arithmetic should still reliably
    # work even with timezone changes (including DST), while likely providing
    # for quicker debugging for users that don't switch timezones frequently.
    fetch_response <- fetch()
    fetch_timestamp <- Sys.time()
  } else {
    if (!silent) {
      message("Cached version of data used.")
    }
    fetch_response = fetch_info$fetch_response
    fetch_timestamp = fetch_info$fetch_timestamp
  }

  check_response(fetch_response)

  if (!is.null(cache_filepath) && should_fetch_now) {
    ## Update cache:
    fetch_info = list(fetch_response=fetch_response, fetch_timestamp=fetch_timestamp)
    if (make_ancestor_directories && !dir.exists(dirname(cache_filepath))) {
      dir.create(dirname(cache_filepath), recursive=TRUE)
    }
    saveRDS(fetch_info, cache_filepath)
  }

  return (fetch_response)
}



.datatable.aware=TRUE

#' @export
epix_truncate_versions_after = function(archive, version) {
  checkmate::assert(epiprocess::is_epi_archive(archive, grouped_okay=TRUE))
  checkmate::assert(identical(class(archive$DT$version), class(version)))

  result = archive$clone()
  result$DT <- result$DT[result$DT$version <= version, colnames(result$DT), with=FALSE]
  if (!is.na(result$clobberable_versions_start) &&
        result$clobberable_versions_start > version) {
    result$clobberable_versions_start <- NA
  }
  result$versions_end <- version

  return(result)
}
