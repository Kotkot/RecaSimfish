treatname.to.standardtreat<-function(treatname) {

  n.treats<-length(treatname)
  standardtreat<-rep("NA",n.treats)
  for (i in 1:n.treats) {
    tn<-treatname[i]
    if (substr(tn,1,9)=="Alpha Max" |
        substr(tn,1,9)=="ALPHA MAX") {
      standardtreat[i]<-"Deltametrin"
    } else if (substr(tn,1,7)=="Betamax") {
      standardtreat[i]<-"Cypermetrin"
    } else if (substr(tn,1,8)=="Salmosan" |
               substr(tn,1,8)=="SALMOSAN" |
               substr(tn,1,7)=="Azasure") {
      standardtreat[i]<-"Azametifos"
    } else if (substr(tn,1,8)=="Hydrogen" |
               substr(tn,1,8)=="HYDROGEN") {
      standardtreat[i]<-"Hydrogenperoksid"
    } else if (substr(tn,1,5)=="Slice" |
               substr(tn,1,9)=="Emamektin" |
               substr(tn,1,9)=="Emamectin") {
      standardtreat[i]<-"Emamektinbenzoat"
    } else if (substr(tn,1,8)=="Ektobann") {
      standardtreat[i]<-"Teflubenzuron"
    } else if (substr(tn,1,7)=="Releeze" |
               substr(tn,1,13)=="Diflubenzuron") {
      standardtreat[i]<-"Diflubenzuron"
    } else if (tn=="Thermolicer" |
               tn=="Optilicer") {
      standardtreat[i]<-"Termisk"
    } else if (tn=="Ferskvann") {
      standardtreat[i]<-"Ferskvann"
    } else if (tn=="Mekanisk" |
               tn=="Hydrolicer") {
      standardtreat[i]<-"Mekanisk"
    } else if (is.na(tn)) {
      standardtreat[i]<-"NA"
    } else {
      standardtreat[i]<-tn
    }
  }

  standardtreat
}
