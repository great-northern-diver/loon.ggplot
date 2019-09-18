loon_title <- function(target) {
  UseMethod('loon_title', target)
}

loon_title.loon <- function(target){
  target["title"]
}

loon_title.l_ts <- function(target) {
  target[[1]]['title']
}
