#' Fitting timeseries models
#'
#' model.timesuperin is used to fit timeseries models.
#' @param data Dataframe containing the history. Must have columns date type and y.
#' @param model.type String 'lm' or 'rlm' to specify a linear or robust limear model
#' @param step.fun Fit feature selection : TRUE, FALSE
#' @param period Data period
#' @param changepoints	Vector of dates at which to include potential changepoints. If not specified, potential changepoints are selected automatically.
#' @param changepoint.prior.scale Parameter modulating the flexibility of the automatic changepoint selection. Large values will allow many changepoints, small values will allow few changepoints.
#' @keywords timesuperin
#' @export

#' @importFrom MASS rlm
#' @import Rcpp
model.timesuperin <-function(data,model.type='lm',formula=NULL,step.fun=F,period=24,
                              changepoints=NULL,
                              changepoint.prior.scale=0.05){
  if (!is.null(formula)){
    formula.timesuperin=as.formula(paste(formula[[2]] , "~" , formula[3],"+time_value","+trend_value"))
  } else {
    formula.timesuperin=as.formula(paste(names(data)[2],"~."))
  }


  data_info<-Data_Handling(data,value=gsub("()","",formula.timesuperin[2]),
                           period=period,changepoints=changepoints,
                           changepoint.prior.scale=changepoint.prior.scale)

  train<-data_info[[1]]
  if(step.fun==T){
    formula.lm<-step(lm(formula.timesuperin,data=train),direction="both")$call$formula
    if(model.type=="rlm"){
      lm_model<-MASS::rlm(formula.lm,data=subset(train, select=-c(time)))
    } else if(model.type=="lm") {
      lm_model<-lm(formula.lm,data=subset(train, select=-c(time)))
    } else {
      warning(paste('model.type=',model.type,"is not supported.","Try 'lm' or 'rlm'" ,"Used model.type default 'lm'"))
    }
    if(formula.lm[[3]][length(formula.lm[[3]])]=="trend_value()"){
      result<-list(model=model.type,
                   formula=formula.lm,
                   lm_model=lm_model,
                   model_summary=summary(lm_model),
                   time_interval=data_info$time_interval,
                   period=data_info$time_period,
                   trend_params=data_info$trend_param,
                   trend=c("TRUE"))
    }else{
      result<-list(model=model.type,
                   formula=formula.lm,
                   lm_model=lm_model,
                   model_summary=summary(lm_model),
                   time_interval=data_info$time_interval,
                   trend=c("FALSE"))
    }
  }else if (step.fun==F){
    formula.lm<-formula.timesuperin
    if(model.type=="rlm"){
      lm_model<-MASS::rlm(formula.lm,data=subset(train, select=-c(time)))
    } else{
      lm_model<-lm(formula.lm,data=subset(train, select=-c(time)))
    }
    result<-list(model=model.type,
                 formula=formula.lm,
                 lm_model=lm_model,
                 model_summary=summary(lm_model),
                 time_interval=data_info$time_interval,
                 period=data_info$time_period,
                 trend_params=data_info$trend_param,
                 trend=c("TRUE"))
  }else{
    warning(paste('step.fun=',step.fun,"is not supported.","Try TRUE or FALSE" ,"Used step.fun default 'FALSE'"))
  }
  return(result)
}



###시간 변수 생성###
#입력데이터들의 형식을 받아 시간 format으로 변경
#' @importFrom stringr str_detect
format_time <- function(data, index='time') {
  if (class(data[[index]])[1] == "POSIXlt" | class(data[[index]])[1] == "POSIXct" ) {
    return(data)
  }
  if (stringr::str_detect(data[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} \\+\\d{4}$")) {
    data[[index]] <- as.POSIXct(strptime(data[[index]], format="%Y-%m-%d %H:%M:%S", tz="UTC"))
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")) {
    data[[index]] <- as.POSIXct(strptime(data[[index]], format="%Y-%m-%d %H:%M:%S", tz="UTC"))
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$")) {
    data[[index]] <- as.POSIXct(strptime(data[[index]], format="%Y-%m-%d %H:%M", tz="UTC"))
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{1}$")) {
    data[[index]] <- as.POSIXct(strptime(data[[index]], format="%Y-%m-%d %H", tz="UTC"))
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{4}-\\d{2}-\\d{2} \\d{2}$")) {
    data[[index]] <- as.POSIXct(strptime(data[[index]], format="%Y-%m-%d %H", tz="UTC"))
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{4}\\d{2}\\d{2} \\d{2}$")) {
    data[[index]] <- as.POSIXct(strptime(data[[index]], format="%Y%m%d %H", tz="UTC"))
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{4}-\\d{2}-\\d{2}$")) {
    data[[index]] <- as.Date(data[[index]], "%Y-%m-%d")
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{2}/\\d{2}/\\d{2}$")) {
    data[[index]] <- as.POSIXct(strptime(data[[index]], format="%m/%d/%y", tz="UTC"))
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{2}/\\d{2}/\\d{4}$")) {
    data[[index]] <- as.POSIXct(strptime(data[[index]], format="%m/%d/%Y", tz="UTC"))
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{4}\\d{2}\\d{2}$")) {
    data[[index]] <- as.Date(data[[index]], "%Y%m%d")
  }
  else if (stringr::str_detect(data[[index]][1], "^\\d{4}/\\d{2}/\\d{2}/\\d{2}$")) {
    data[[index]] <- as.POSIXct(strptime(data[[index]], format="%Y/%m/%d/%H", tz="UTC"))
  }
  else if( stringr::str_detect(data[[index]][1],"^\\d{4}-\\d{2}$")){
    data[[index]] <- as.Date(paste0((data[[index]]),"-01"),"%Y-%m-%d")
  }
  else if( stringr::str_detect(data[[index]][1],"^\\d{4}/\\d{2}$")){
    data[[index]] <- as.Date(paste0((data[[index]]),"/01"),"%Y/%m/%d")
  }

  return(data)
}


###연,월,일,시,분,초 구분 함수###
#입력데이터의 시간 간격을 구분
get_gran = function(data, index='time') {
  n = length(data[[index]])
  gran = round(difftime(max(data[[index]]),sort(data[[index]], partial=n-1)[n-1], units="secs"))
  if (gran >= 2419200){
    return("mon")
  }
  else if (gran >= 86400) {
    return("day")
  }
  else if (gran >= 3600) {
    return("hr")
  }
  else if (gran >= 60) {
    return("min")
  }
  else if (gran >= 1) {
    return("sec")
  }
}


###시간변수 생성###
#구분된 시간간격을 이용하여 시간변수 생성
#시간 구분이 월단위 : 01~12
#시간 구분이 일단위 : 01~31
#시간 구분이 시간단위 : 01~24
#시간 구분이 초단위 : 01~60
timevalue<-function(data){
  if (get_gran(data)=="mon"){
    return(substr(data[['time']],6,7))
  }
  else if (get_gran(data)=="day"){
    return(substr(data[['time']],9,10))
  }
  else if (get_gran(data)=="hr"){
    return(substr(data[['time']],12,13))
  }
  else if (get_gran(data)=="min"){
    return(substr(data[['time']],15,16))
  }
  else if (get_gran(data)=="sec"){
    return(substr(data[['time']],18,19))
  }
}


###Data Handling###
###데이터 핸들링 및 변수 생성 함수###
#트랜드 변수 생성 여부에 따라 사용 옵션이 달라짐
#data : 학습데이터
#data의 시간값을 가지는 컬럼명은 time으로 지정해줘야함
#period : 데이터의 주기
#changepoints : 데이터의 특성 변경점 , 데이터의 시간단위와 일치
#changepoint.prior.scale : 변경점 선택의 유연성을 조정하는 변수, 큰값은 많은수를 작은값은 적은수의 변경점 허용

#' @importFrom dplyr arrange
Data_Handling<-function(data,period=NULL,changepoints=NULL,value=NULL,
                        n.changepoints=NULL,changepoint.prior.scale=NULL){

  #시간데이터 입력 형식에 따른 시간 포맷으로 변경
  data<-format_time(data)
  #data[['time']]<-as.POSIXct(data[['time']])
  data <- dplyr::arrange(data,data[['time']])
  #트랜드 변수 생성#
  if(!is.null(changepoints)){
    n.changepoints<-length(changepoints)
  }else{
    n.changepoints<-c(25)
  }
  m <- list(
    value=value,
    period = period,
    changepoints = changepoints,
    n.changepoints = n.changepoints,
    seasonality.prior.scale = 10,
    changepoint.prior.scale = changepoint.prior.scale,
    y.scale = NULL,
    t.scale = NULL,
    changepoints.t = NULL,
    params = list(),
    history = data
  )

  m<-mk.trend.parm(m)
  data<-data.frame(data,time_value=as.factor(timevalue(data)),
                   trend_value=predict_trend(m,data))

  #names(data)[2]<-c('value')
  return(list(data=data,time_interval=get_gran(data),time_period=period,
              trend_param=list(params=m$params,start=m$start,t.scale=m$t.scale,
                               y.scale=m$y.scale,changepoints=m$changepoints,changepoints.t=m$changepoints.t)))

}




#######################################예측#########################################
#예측에 사용될 데이터 테이블 생성#
#예측에 필요한 데이터를 정제시키는 함수
#data : 예측 데이터
#data의 시간값을 가지는 컬럼명은 time으로 지정해줘야함
#trend_param : 예측시 사용되는 트랜드 생성 모수
make.target_data<-function(data,trend=NULL,trend_param=NULL){
  #시간데이터 입력 형식에 따른 시간 포맷으로 변경
  data<-format_time(data)
  #트렌드변수 생성
  if(trend==T){
    data<-data.frame(data,time_value=as.factor(timevalue(data)),
                     trend_value=predict_trend(trend_param,data))
  } else {
    data<-data.frame(data,time_value=as.factor(timevalue(data)))
  }
  return(data)
}


#예측#
#정제된 예측데이터를 입력하여 목표가되는 시간대의 예측값 및 예측값의 상한값, 하한값을 생성
#object : 모델생성 함수의 결과, 생성된 모델 및 트랜드 파라미터 정보
#newdata : 정제된 예측데이터
#level : 예측 신뢰구간의 예측 수준

#' Model predictions
#'
#' This function for predictions from the results of model fitting functions.
#' @param object result of model fitting functions.
#' @param newdata dataframe for predictions. Must have columns date type.
#' @param level Tolerance/confidence level.
#' @keywords predict
#' @export
pred.table.timesuperin<-function(object,newdata,level=0.95){

  if(object$trend==T){
    target<-make.target_data(newdata,trend=object$trend,
                             trend_param=object$trend_params)
  }else if(object$trend==F){
    target<-make.target_data(newdata,trend=object$trend)
  }
  predic.table<-data.frame(time=target[['time']],
                           predict(object$lm_model,newdata=target , interval='prediction',level=level))
  return(predic.table)
}


###output 생성 : 모델 summary & 전체 생성 값 출력 & outlier 탐지값 & plot###
#목표가 되는 시간대의 생성된 예측값 및 상한값, 하한값 활용하여 이상탐지
#object : 모델 생성 함수의 결과, 생성된 모델 및 트랜드 파라미터 정보
#direction : 탐지 방향, upr일경우 상한값을 넘는 이상치들만 탐색, lwr일경우 하한값을 넘는 이상치들만 탐색
#both일 경우 상한값과 하한값 모두 넘는 이상치들 탐색
#level : 탐지에 사용된 예측 신뢰구간의 수준
#cumul.thre : 경험적 수치로 지정하는 누적잔차 기준
#결과는 탐지된 이상치 테이블, 상한,하한,예측값의 plot, 실제값과 예측값의 차이인 잔차의 누적합과 상한,하한plot으로 구성

#' Detect anomaly data
#'
#' Detection for anomaly data based on predcition interval
#' @param object result of model fitting functions.
#' @param newdata dataframe for predictions. Must have columns date type.
#' @param level Tolerance/confidence level.
#' @param value Anormal confirmation data.
#' @param direction Anomaly detection direction : 'upper', 'lower', 'both'
#' @param cumul.thre threshold for Cumulative residual
#' @keywords Detection
#' @export

detect_anormal.timesuperin<-function(object,newdata,level=0.95,value,direction='both',cumul.thre=NULL){

  predic.table<-pred.table.timesuperin(object,newdata,level=level)
  #신뢰구간, 신뢰수준 기본값은 prediction & 0.95
  interval.type='prediction'
  #outlier탐지
  predic.table$value<-value
  level<-level
  #names(predic.table)[ncol(predic.table)]<-c('value')

  #' @import ggplot2
  #' @importFrom reshape melt
  #하한,상한, 예측값, 실제값 plot
  predic.table.melt<-reshape::melt(predic.table[,c('time','value','fit','upr','lwr')],id.vars='time')
  plot.ex<-ggplot2::ggplot(predic.table.melt,ggplot2::aes(time,value,group=variable,col=variable))+
    ggplot2::geom_point()+ggplot2::geom_line()+
    ggplot2::theme_bw()+ggplot2::theme(axis.title.y = ggplot2::element_text(face='bold', angle = 90),
                                       plot.title = ggplot2::element_text(size = ggplot2::rel(1.5),face = 'bold',hjust = 0.5))+
    ggplot2::scale_y_continuous(labels=scales ::comma)+
    ggplot2::ggtitle(paste(level,"level","Prediction","Interval"))


  #누적 잔차 plot
  #여기서 누적 잔차는 상한, 하한을 넘지 않는 값들의 잔차를 누적
  predic.table$residual<-predic.table$value - predic.table$fit
  predic.table$residual_for_cumul<-predic.table$residual
  predic.table$residual_for_cumul[predic.table$value < predic.table$lwr |
                                    predic.table$value > predic.table$upr]<-c(0)

  predic.table$cumulative_sum_of_residual<-cumsum(predic.table$residual_for_cumul)

  if(!is.null(cumul.thre)){
    predic.table$cumul_upr<-0+cumul.thre
    predic.table$cumul_lwr<-0-cumul.thre
  }else {
    predic.table$cumul_upr<-0+qnorm(1-(1-level)/8)*sd(predic.table$cumulative_sum_of_residual)
    predic.table$cumul_lwr<-0-qnorm(1-(1-level)/8)*sd(predic.table$cumulative_sum_of_residual)
  }


  predic.table.melt<-reshape::melt(predic.table[,c('time','cumulative_sum_of_residual',
                                                   'cumul_upr','cumul_lwr')],id.vars='time')
  plot.resid.ex<-ggplot2::ggplot(predic.table.melt,ggplot2::aes(time,value,group=variable,col=variable))+
    ggplot2::geom_point()+ggplot2::geom_line()+
    ggplot2::theme_bw()+ggplot2::theme(axis.title.y = ggplot2::element_text(face='bold', angle = 90),
                                       plot.title = ggplot2::element_text(size = ggplot2::rel(1.5),face = 'bold',hjust = 0.5))+
    ggplot2::scale_y_continuous(labels=scales ::comma)+
    ggplot2::ggtitle("Cumulative Sum of Residual ")+ggplot2::ylab("Cumulative Sum of Residual")

  #이상치 판별
  if(direction=="upr"){
    predic.table$anormal_flag<-c('normal')
    predic.table$anormal_flag[predic.table$upr < predic.table$value]<-c('upr_anormal')
  }else if(direction=='lwr'){
    predic.table$anormal_flag<-c('normal')
    predic.table$anormal_flag[predic.table$lwr > predic.table$value]<-c('lwr_anormal')
  }else if(direction=='both'){
    predic.table$anormal_flag<-c('normal')
    predic.table$anormal_flag[predic.table$lwr > predic.table$value ]<-c('lwr_anormal')
    predic.table$anormal_flag[predic.table$upr < predic.table$value ]<-c('upr_anormal')
  }else {
    warning(paste('direction=',direction,"is not supported.","Try 'upr' or 'lwr' or 'both'.","Used direction default 'both'" ))
  }
  predic.table$anormal_flag[predic.table$cumul_lwr > predic.table$cumulative_sum_of_residual|
                              predic.table$cumul_upr < predic.table$cumulative_sum_of_residual  ]<-c('cum_resid_anormal')

  predic.table$anormal_flag<-as.factor(predic.table$anormal_flag)


  return(list("result_table"=predic.table[,-7],
              "Interval_Plot"=plot.ex,"Cumulative_Sum_of_Residual_Plot"=plot.resid.ex))
}


######################################################################################################
########트렌드 생성#######
#seasonality features 생성
#data : 학습데이터
#period : 데이터 주기
#series.order : 구성요소의 수
#seasonality features matrix return
fourier_series <- function(data, period, series.order) {
  if (get_gran(data)=='day'){
    t <- data[['time']] - zoo::as.Date('1970-01-01')

  }
  else if (get_gran(data)=='hr'){
    t <- as.numeric(difftime(data[['time']] ,
                             as.POSIXct(strptime('1970-01-01 00', format="%Y-%m-%d %H", tz="UTC"),units = c('days'))))
  }
  features <- matrix(0, length(t), 2 * series.order)
  for (i in 1:series.order) {
    x <- as.numeric(2 * i * pi * t / period)
    features[, i * 2 - 1] <- sin(x)
    features[, i * 2] <- cos(x)
  }
  return(features)
}

#seasonality features matrix 생성
#prefix : 컬럼명 첨자
make_seasonality_features <- function(data, period, series.order, prefix) {
  features <- fourier_series(data, period, series.order)
  colnames(features) <- paste(prefix, 1:ncol(features), sep = '_delim_')
  return(data.frame(features))
}

# seasonality features data frame 생성
#m : prophet trend 생성을 위한 object
#data : 학습데이터

make_all_seasonality_features <- function(m, data) {
  seasonal.features <- data.frame(zeros = rep(0, nrow(data)))
  if (m$period > 7 & get_gran(data)=='day') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 10, 'yearly'))
  }  else if (m$period ==7 & get_gran(data)=='day') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 3, 'weekly'))
  }  else if (m$period > 24 & get_gran(data)=='hr') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 10, 'hourly'))
  } else if (m$period <= 24 & get_gran(data)=='hr') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 3, 'hourly'))
  }else if (m$period > 1 & get_gran(data)=='mon') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 10, 'monthly'))
  } else if (m$period <= 1 & get_gran(data)=='mon') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 3, 'monthly'))
  }else if (m$period > 60 & get_gran(data)=='min') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 10, 'monthly'))
  } else if (m$period <= 60 & get_gran(data)=='min') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 3, 'monthly'))
  }else if (m$period > 60 & get_gran(data)=='sec') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 10, 'monthly'))
  } else if (m$period <= 60 & get_gran(data)=='sec') {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(data, m$period, 3, 'monthly'))
  }
  return(seasonal.features)
}

#트랜드 변수 생성을 위한 준비 테이블
#m : prophet trend 생성을 위한 object
#data : 트랜드 생성전 준비가 필요한 데이터로 학습데이터 또는 예측데이터
#run.ex : 학습데이터일 경우 TRUE

setup_dataframe <- function(m, data,run.ex=F) {
  value=m$value
  if (ncol(data)>1) {
    data[[value]] <- as.numeric(data[[value]])
  }

  if (anyNA(data[['time']])) {
    stop('Unable to parse date format in column time. Convert to date format.')
  }

  if(run.ex==T){
    m$y.scale <- max(data[[value]])
    m$start <- min(data[['time']])
    m$t.scale <- as.numeric(difftime(max(data[['time']]) , min(data[['time']]),units = c('days')))
  }

  data$t <- as.numeric(difftime(data[['time']] , m$start,units = c('days')) / m$t.scale)
  if (ncol(data)>1) {
    data$y_scaled <- data[[value]] / m$y.scale
  }

  return(list("m" = m, "data" = data))
}

# 변경점 지정
#m : prophet trend 생성을 위한 object
set_changepoints <- function(m) {
  if (!is.null(m$changepoints)) {
    if (length(m$changepoints) > 0) {
      if (min(m$changepoints) < min(m$history[['time']])
          || max(m$changepoints) > max(m$history[['time']])) {
        stop('Changepoints must fall within training data.')
      }
    }
    if (get_gran(m$history)=='day'){

      m$changepoints <- m$history$time[m$history$time>= zoo::as.Date(m$changepoints)]
      m$changepoints.t <- sort(as.numeric(m$changepoints - m$start) / m$t.scale)

    }else if(get_gran(m$history)=='hr'){

      m$changepoints <- m$history$time[m$history$time>= format_time(m,index ='changepoints' )[['changepoints']]]
      m$changepoints.t <- sort(as.numeric(difftime(m$changepoints , m$start,units = c('days')) / m$t.scale))

    }else if(get_gran(m$history)=='mon'){

      m$changepoints <- m$history$time[m$history$time>= format_time(m,index ='changepoints' )[['changepoints']]]
      m$changepoints.t <- sort(as.numeric(difftime(m$changepoints , m$start,units = c('days')) / m$t.scale))
    }else if(get_gran(m$history)=='sec'){

      m$changepoints <- m$history$time[m$history$time>= format_time(m,index ='changepoints' )[['changepoints']]]
      m$changepoints.t <- sort(as.numeric(difftime(m$changepoints , m$start,units = c('days')) / m$t.scale))
    }else if(get_gran(m$history)=='min'){

      m$changepoints <- m$history$time[m$history$time>= format_time(m,index ='changepoints' )[['changepoints']]]
      m$changepoints.t <- sort(as.numeric(difftime(m$changepoints , m$start,units = c('days')) / m$t.scale))
    }
  } else {
    if (m$n.changepoints > 0) {
      # Place potential changepoints evenly through the first 80 pcnt of
      # the history.
      cp.indexes <- round(seq.int(1, floor(nrow(m$history) * .8),
                                  length.out = (m$n.changepoints + 1)))[-1]
      m$changepoints <- m$history[['time']][cp.indexes]


      if (get_gran(m$history)=='day'){
        if (length(m$changepoints) > 0) {
          m$changepoints <- zoo::as.Date(m$changepoints)
          m$changepoints.t <- sort(as.numeric(m$changepoints - m$start) / m$t.scale)
        }
      }else if(get_gran(m$history)=='hr'){
        if (length(m$changepoints) > 0) {
          m$changepoints <- format_time(m,index ='changepoints' )[['changepoints']]
          m$changepoints.t <- sort(as.numeric(difftime(m$changepoints , m$start,units = c('days')) / m$t.scale))
        }
      }else if(get_gran(m$history)=='mon'){
        if (length(m$changepoints) > 0) {
          m$changepoints <- format_time(m,index ='changepoints' )[['changepoints']]
          m$changepoints.t <- sort(as.numeric(difftime(m$changepoints , m$start,units = c('days')) / m$t.scale))
        }
      }else if(get_gran(m$history)=='min'){
        if (length(m$changepoints) > 0) {
          m$changepoints <- format_time(m,index ='changepoints' )[['changepoints']]
          m$changepoints.t <- sort(as.numeric(difftime(m$changepoints , m$start,units = c('days')) / m$t.scale))
        }
      }else if(get_gran(m$history)=='sec'){
        if (length(m$changepoints) > 0) {
          m$changepoints <- format_time(m,index ='changepoints' )[['changepoints']]
          m$changepoints.t <- sort(as.numeric(difftime(m$changepoints , m$start,units = c('days')) / m$t.scale))
        }
      }

    } else {
      m$changepoints <- c()
    }
  }

  return(m)
}




# 변경점 이전은 0, 이후는 1인 값을 가지는 matrix 생성
#m : prophet trend 생성을 위한 object
get_changepoint_matrix <- function(m) {
  A <- matrix(0, nrow(m$history), length(m$changepoints.t))
  for (i in 1:length(m$changepoints.t)) {
    A[m$history$t >= m$changepoints.t[i], i] <- 1
  }
  return(A)
}

# 시계열의 첫번째와 마지막 포인트를 이용한 트랜드 생성에 필요한 초기 parameter 생성
#df : 학습데이터
linear_growth_init <- function(df) {
  i0 <- which.min(as.POSIXct(df[['time']]))
  i1 <- which.max(as.POSIXct(df[['time']]))
  T <- df$t[i1] - df$t[i0]
  # Initialize the rate
  k <- (df$y_scaled[i1] - df$y_scaled[i0]) / T
  # And the offset
  m <- df$y_scaled[i0] - k * df$t[i0]
  return(c(k, m))
}

# 확률함수 모델인 Stan model compile
#model : linear , linear trend parameter 생성 모델 compile
get_prophet_stan_model <- function(model) {
  fn <- paste('prophet', model, 'growth.RData', sep = '_')
  ## If the cached model doesn't work, just compile a new one.
  tryCatch({
    binary <- system.file('libs', Sys.getenv('R_ARCH'), fn,
                          package = 'timesuperin',
                          mustWork = TRUE)
    load(binary)
    obj.name <- paste(model, 'growth.stanm', sep = '.')
    stanm <- eval(parse(text = obj.name))

  })
}

# 최종 트랜드를 생성하는 parameter 생성
#m : prophet trend 생성을 위한 object
mk.trend.parm<-function(m){

  #트랜드 변수 생성을 위한 준비 테이블
  out <- setup_dataframe(m, m$history,run.ex=T)
  history <- out$data
  m <- out$m
  m$history <- history

  # seasonality features data frame 생성
  seasonal.features <- make_all_seasonality_features(m, history)

  # 변경점 지정과 marix 생성
  m <- set_changepoints(m)
  A <- get_changepoint_matrix(m)

  # stan model의 입력값 지정
  dat <- list(
    T = nrow(history),
    K = ncol(seasonal.features),
    S = length(m$changepoints.t),
    y = history$y_scaled,
    t = history$t,
    A = A,
    t_change = array(m$changepoints.t),
    X = as.matrix(seasonal.features),
    sigma = m$seasonality.prior.scale,
    tau = m$changepoint.prior.scale
  )

  model <- get_prophet_stan_model('linear')

  stan_init <- function() {
    list(k = linear_growth_init(history)[1],
         m = linear_growth_init(history)[2],
         delta = array(rep(0, length(m$changepoints.t))),
         beta = array(rep(0, ncol(seasonal.features))),
         sigma_obs = 1
    )
  }

  # stan model을 이용하여 트랜드 parameter 추정
  stan.fit <- rstan::optimizing(
    model,
    data = dat,
    init = stan_init,
    iter = 1e4,
    as_vector = FALSE
  )
  m$params <- stan.fit$par
  return(m)
}


# 트랜드 생성(예측)
#t : 트랜드 생성 목표 데이터의 (현재 시간 - 데이터 시작 시간) / (데이터 시작 시간 - 데이터 끝시간)
#deltas : 생성된 변경점의 변화율
#m : prophet trend 생성을 위한 object
#changepoint.ts : (변경점 시간 - 데이터 시작 시간) / (데이터 시작 시간 - 데이터 끝시간)
piecewise_linear <- function(t, deltas, k, m, changepoint.ts) {
  # Intercept changes
  gammas <- -changepoint.ts * deltas
  # Get cumulative slope and intercept at each t
  k_t <- rep(k, length(t))
  m_t <- rep(m, length(t))
  for (s in 1:length(changepoint.ts)) {
    indx <- t >= changepoint.ts[s]
    k_t[indx] <- k_t[indx] + deltas[s]
    m_t[indx] <- m_t[indx] + gammas[s]
  }
  y <- k_t * t + m_t
  return(y)
}


# 트랜드 생성(예측)
#trend_param : 트랜드 생성에 필요한 파라미터, 모델 생성 함수에서 발생
#k	: 실제값의 기본 성장율 , ((마지막 실제값 - 첫번째 실제값)/가장큰 실제값))
#m	: offset, 가장큰 실제값대비 첫번째 실제값의 비율
#delta :	변경점의 성장 변화율
#beta : seasonal vector
#gamma	: 수정된 offset, changepoints와 delta의 곱
#t.scale	 : 시간순으로 가장마지막과 가장첫번째의 시간 차이
#y.scale	: 가장큰 실제값
#changepoints.t	: (변경점 시간 - 데이터 시작 시간) / (데이터 시작 시간 - 데이터 끝시간)
predict_trend <- function(trend_param, df) {
  df$t <- as.numeric(difftime(df[['time']] , trend_param$start,units = c('days')) / trend_param$t.scale)
  k <- mean(trend_param$params$k, na.rm = TRUE)
  param.m <- mean(trend_param$params$m, na.rm = TRUE)
  deltas <- trend_param$params$delta
  t <- df$t
  trend <- piecewise_linear(t, deltas, k, param.m, trend_param$changepoints.t)
  return(trend * trend_param$y.scale)
}
