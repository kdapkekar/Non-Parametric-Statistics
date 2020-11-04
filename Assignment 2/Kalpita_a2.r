
JackKnife = function (x, theta, ...) 
{
  call = match.call()
  num = length(x)
  y = rep(0, num)
  for (i in 1:num) {
    y[i] = theta(x[-i], ...)
  }
  theta.hat = theta(x, ...)
  pseudo.values = num*theta.hat - (num-1)*y
  theta.jack = mean(pseudo.values)
  jack.se = sqrt(sum((pseudo.values - theta.jack)^2)/(num*(num-1)))
  jack.bias = (num-1)*(theta.hat - mean(u))
  return(list(theta.hat = theta.hat,       
              theta.jack = theta.jack,     
              jack.bias = jack.bias,
              jack.se = jack.se,            
              leave.one.out.estimates = y, 
              pseudo.values = pseudo.values,
              call = call))
}
