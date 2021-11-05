round_half_up_to <- function(x, base) {
  janitor::round_half_up(x / base, 0) * base
}