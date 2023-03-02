pre.OSdata <- function(data, cut.time=5) {
  # data
  # os  time
  # 1   5.7
  # 0   3
  # 1   7.1
  # cut.time后发生结局事件（死亡，1）的数据改为截尾数据（0），调整为（0，cut.time）
  # cut.time后仍然未发生结局事件（0）只需要调整为cut.time即可
  index = (data[,1] == 1) & (data[,2] > cut.time)
  data[index, 1] <- 0
  data[index, 2] <- cut.time
  #
  data[data[,2] > cut.time, 2] <- cut.time
  return(data)
}
