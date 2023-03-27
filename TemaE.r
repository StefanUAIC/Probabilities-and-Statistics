Ex1=function(tip_test, x, miu, s, n, alfa)
{
  t_score = (x - miu) / (s / sqrt(n))
  if (tip_test == "right") {
    critical_t = qt(alfa, n - 1)
    if (t_score > critical_t) {
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.")
    }
    else {
      print("Nu avem suficiente dovezi.\n")
    }
  }
  if (tip_test == "left") {
    critical_t = qt(1 - alfa, n - 1)
    print(paste("critical_t = ", critical_t))
    if (t_score < critical_t) {
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.")
    }
    else {
      print("Nu avem suficiente dovezi.\n")
    }
  }
  if (tip_test == "simetric") {
    critical_t = qt(1 - alfa / 2, n - 1)
    print(paste("critical_t = ", critical_t))
    if (abs(t_score) < abs(critical_t)) 
    {
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.")
    }
    else 
    {
      print("Nu avem suficiente dovezi.")
    }
  }
}
Ex1("left", 418, 420, 2.75, 125, 0.01)


Ex2 = function(n, sample_mean, population_mean, sigma, alfa, type)
{
  z_score = (sample_mean - population_mean) / (sigma / sqrt(n))
  if(type == "left") {
    critical_z = qnorm(alfa, 0, 1)
    if(z_score < critical_z) {
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.")
    }
    else
      print("Nu avem suficiente dovezi.")
  }
  if(type == "right") {
    critical_z = qnorm(1 - alfa, 0, 1)
    if(z_score > critical_z) {
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.");
    }
    else
      print("Nu avem suficiente dovezi.")
  }
  if(type == "simetric") {
    critical_z = -qnorm(alfa / 2, 0, 1)
    if(abs(z_score) > abs(critical_z)) {
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.");}
    else
      print("Nu avem suficiente dovezi.")
  }
}
Ex2(25, 5.17, 4.9, 0.35, 0.01, "right")
Ex2(25, 5.17, 4.9, 0.35, 0.05, "right")

Ex3 = function(n1, n2, sm1, sm2, m0, sigma1, sigma2, alfa, type)
{
  z_score = ((sm1 - sm2) - m0) / (sqrt(sigma1 ^ 2 / n1 + sigma2 ^ 2 / n2))
  if(type == "left")
  {
    critical_z = qnorm(alfa, 0, 1)
    if (z_score < critical_z)
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.")
    else
      print("Nu avem suficiente dovezi.")
  }
  if (type == "right")
  {
    critical_z = qnorm(1 - alfa, 0, 1)
    if (z_score > critical_z)
    {print("Ipoteza nula respinsa, se accepta ipoteza alternativa.");}
    else
      print("Nu avem suficiente dovezi.")
  }
  if (type == "simetric")
  {
    critical_z = -qnorm(alfa/2, 0, 1)
    if (abs(z_score) > abs(critical_z))
    {
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.");
    }
    else
      print("Nu avem suficiente dovezi.")
  }
}

#Subpunctul a
Ex3(25, 28, 5.48, 6.12, 0, 1.31, 0.93, 0.01, "simetric")

#Subpunctul b
Ex3(25, 28, 5.48, 6.12, 0, 1.31, 0.93, 0.01, "left")



Ex4 = function(type, s1, s2, n1, n2, alfa)
{
  F_score = s1 ^ 2 / s2 ^ 2
  if(type == "simetric") {
    critical_F_s = qf(alfa / 2, n1 - 1, n2 - 1)
    critical_F_d = qf(1 - alfa / 2, n1 - 1, n2 - 1)
    if(F_score < critical_F_s | F_score > critical_F_d) {
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.")
    }
    else {
      print("Nu avem suficiente dovezi.")
    }
  }
  if(type == "right") {
    critical_F = qf(1 - alfa, n1 - 1, n2 - 1)
    if(F_score > critical_F){
      print("Ipoteza nula respinsa, se accepta ipoteza alternativa.")
    }
    else
    {
      print("Nu avem suficiente dovezi.")
    }
  }
}
Ex4("right", 1.24, 0.87, 25, 28, 0.01)

