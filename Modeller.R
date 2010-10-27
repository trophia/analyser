Modeller <- Worker$proto(
  terms = expression(~fyear)
)

Modeller$do = function(.,data){
  .$model = glm(catch~fyear,data=data)
  return(.$model)
}