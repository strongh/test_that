if (!exists("Reporter")) {
  Reporter <- mutatr::Object$clone()
    SummaryReporter <- Reporter$clone()
    LogReporter <- Reporter$clone()
    StopReporter <- Reporter$clone()  
    MinimalReporter <- Reporter$clone()  
}
