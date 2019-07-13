function(time, distance = 5, goal = 42.2, exponent = 1.06) {
  time = as.numeric(time)
  time * (goal / distance) ** exponent
}

riegel(25)