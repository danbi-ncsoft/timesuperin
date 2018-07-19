# Timesuperin

Timesuperin 은 '**Time Superin**tendent (시간 관리자)'의 줄임말로써 시계열 데이터 분석 및 이상 탐지를 위한 라이브러리입니다.
시계열 지표를 이벤트 정보와 트렌드를 고려하여 회귀 모델을 생성하여 향후 지표의 변화를 추정하고, 이렇게 추정된 결과와 크게 차이가 발생하는 이상 현상을 탐지하는 기능을 제공합니다. 

## 설치 방법
timesuperin을 사용하기 위해선 먼저 아래 라이브러리를 설치해야 합니다.
* Rcpp
* rstan
* BH

위 라이브러리가 설치된 상태에서 아래 코드를 실행하시면 됩니다.

    devtools::install_github("ncsoft/timesuperin")

## 사용 방법
모델링 및 시계열 이상 탐지 방법은 아래와 같습니다. 
먼저 timesuperin/resources 에 있는 데이터를 불러옵니다.

	setwd('./timesuperin/resources')
	train_data <- read.csv('./train_data.csv')
	test_data <- read.csv('./test_data.csv')

train_data.csv 와 test_data.csv 는 두 개의 이벤트 변수에 영향을 받는 시계열 데이터입니다.
모델링에 사용할 학습 데이터는 아래 그림과 같습니다. 

![](https://raw.githubusercontent.com/danbi-ncsoft/timesuperin/master/resources/train_data.png)

이제 위 데이터를 이용해 아래와 같이 시계열 모델을 생성합니다.

	model <- model.timesuperin(train_data, model.type = 'lm', period = 6)

위 모델을 이용해 테스트 데이터에 있는 이상 데이터를 탐지하기 위해 아래와 같이 detect_anomal.timesuperin 함수를 실행합니다.

	anomaly.detect <- detect_anomal.timesuperin(model, test_data, value = test_data$value)

이상 탐지 결과는 다음과 같이 확인할 수 있습니다 (두 개의 데이터가 upr과 lwr 범위를 벗어난 것을 확인할 수 있습니다).

	result$Interval_Plot

![](https://raw.githubusercontent.com/danbi-ncsoft/timesuperin/master/resources/anomaly_detect.png)
