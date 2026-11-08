# Presentation only: add scale-training values without dropping any observations.
# Each facet gets its own starting-level envelope; real extremes expand it.
natural_axis_bounds <- function(d,value='value',time='year',panels=character(),
                                signed=FALSE,zero=FALSE,floor=1) {
  groups<-if(length(panels))interaction(d[panels],drop=TRUE,lex.order=TRUE) else rep(1,nrow(d))
  out<-lapply(split(d,groups),function(z){
    v<-z[[value]];ok<-is.finite(v)
    first<-if(time%in%names(z))z[[time]]==min(z[[time]][ok],na.rm=TRUE) else rep(TRUE,nrow(z))
    base<-v[ok & first];base<-base[is.finite(base)]
    if(!length(base))base<-0
    if(signed || any(base<0)) {
      magnitude<-max(abs(base),floor/2)
      limits<-c(-2*magnitude,2*magnitude)
    } else if(max(base)==0) limits<-c(0,floor)
    else limits<-c(if(zero)0 else min(base)/2,1.5*max(base))
    b<-z[rep(1,2),panels,drop=FALSE];b$.axis_value<-limits;b
  })
  do.call(rbind,out)
}
natural_y <- function(d,value='value',time='year',panels=character(),signed=FALSE,zero=FALSE,floor=1) {
  ggplot2::geom_blank(data=natural_axis_bounds(d,value,time,panels,signed,zero,floor),
    mapping=ggplot2::aes(y=.axis_value),inherit.aes=FALSE,show.legend=FALSE)
}
# Contributions are signed changes: use a zero-centred minimum span rather than
# a ratio to their own (often tiny) values. Expand automatically for larger bars.
natural_contribution_axis <- function(baseline,axis='y',minimum=1) {
  radius<-max(abs(baseline[is.finite(baseline)]),minimum)*.5
  limits<-function(r)range(r,-radius,radius,finite=TRUE)
  if(axis=='x')ggplot2::scale_x_continuous(limits=limits)
  else ggplot2::scale_y_continuous(limits=limits)
}
