(* ::Package:: *)

(* Mathematica Package *)
(* set the config params for the epsr system *)
(* Copyrrigth JP Tollenboom 2011 *)
(* !!! This file must be loaded at every query !!! *)

(* version history *)
(* path setter changed to universal *)
(* jan 2013: added dateIndCorr *)
(* sep 2013: made for SMSHAR *)
(* March 2014: modified smshMaxtaskCount control *)
(* Jan 2015: replaed $RootDirectory/... by $HomeDirectory *)

BeginPackage["smsharConfig`"]


(* ::Section:: *)
(* paths *)


epsrSMSHARData::usage = "Root";
epsrResourcesPath::usage = "path to resources";
epsrRepPath::usage = "for reports";(* not so safe *)
epsrTicketsPath::usage = "path to tickets";
epsrReportDataPath::usage="path to reportData copies for ticket handler";
epsrCDFReportsPath::usage = "path to reports";
epsrLogsPath::usage = "path to app logs";
epsrCavernPath::usage = "path to dpc repo volume";
epsrDomainsPath::usage = "";

epsrSMTPServer::usage = "";

(* for the cdf maker *)
datadir::usage="";
reportsdir::usage="";
templatesdir::usage="";


(* ::Section:: *)
(* Exported symbols *)


epsrValidActions::usage = "the list of valid actions; is defined per class of user ";
epsrMCPageTemplate::usage = "the template of the MC page";
epsrValidActionTranslation::usage = "to translate epsrParam values into displayed names; lan dependant";
dateIndex::usage = "['dd-mm-yyyy']; returns index";
revDateIndex::usage = "[index];returns 'dd-mm-yyyy'";
hdrsDiagnostics::usage = "headers for the diagnostics ";
epsrPutAndEncodeFile::usage = "[object,fileName];Puts and encodes a file, given it's file path";
epsrStateDefault::usage = "the default state file";
epsrAdv::usage = "";

projectClasses::usage = "{{class name, index}}";
taskCountLimits::usage = "{{class index, taskcount limit}}";
maxTaskCount::usage="defined as function of size ";
allowedPlanningSystems::usage = "{{full name, short name}}";
allowedFileExtension::usage = "{{short name,{file extensions}}}";
dateIndCorr::usage=="is set to 0 for normal cases; is calculatated in the portfolio report";

smshColnamstranslator::usage = "maps the colnames obtained through fetch function on std epsr col names";

(*smshClass::usage = "";
smshMaxTaskCount::usage = "";*)

smshTimeLimit::usage = "";
smshAccessToken::usage = "";
smshAccSheetID::usage = "";
smshProjSheetID::usage = "";
flagIndex::usage="";
smshMaxlevelforaudit::usage="";

(* for the cdf maker *)
datadir::usage="";
reportsdir::usage="";
templatesdir::usage="";



(* ::Section:: *)
(* Debugging *)


Begin["`Private`"] 


(* ::Section:: *)
(* SETTING THE PATHS *)


(* +++ Universal Path setting +++ *)
macName = StringReplace[$MachineName, "-" -> " "];
hdName = macName<>" HD";

(* engine bound paths *)
epsrSMSHARData = FileNameJoin[{$HomeDirectory,"Dropbox","SMSHAR World","SMSHARDATA"}];
epsrDomainsPath = FileNameJoin[{$HomeDirectory,"Dropbox","Public","SMSHAR"}];
epsrResourcesPath = epsrSMSHARData;
epsrRepPath = FileNameJoin[{epsrDomainsPath,"Reports"}];(* new *)
(* ticketting *)
epsrTicketsPath = FileNameJoin[{$HomeDirectory,"Dropbox","Ticketsmachine","tickets"}];
epsrReportDataPath=FileNameJoin[{$HomeDirectory,"Dropbox","Ticketsmachine","data"}];

epsrCDFReportsPath = FileNameJoin[{epsrSMSHARData,"Reports"}];(* old *)
epsrLogsPath = FileNameJoin[{epsrSMSHARData,"logs"}];
epsrCavernPath = epsrSMSHARData;

(* cdf's making stuff *)
datadir=FileNameJoin[{$HomeDirectory,"Dropbox","SMSHAR World","SMSHARDATA","Reports"}];
reportsdir=FileNameJoin[{$HomeDirectory,"Dropbox","Public","SMSHAR","Reports"}];
templatesdir=FileNameJoin[{$HomeDirectory,"Dropbox","SMSHAR World","SMSHARDATA","Resources"}];

(* +++ END SETTING THE PATHS +++ *)


(* MAIL SERVER *)
(* personnal *)
epsrSMTPServer = "SMTP.Telenet.be";

(* project class parameters *)
(* fixed for all SMSHAR projects *)
(*smshClass = 1;*)
(* removed and replaced by account dependant value *)
(*
smshMaxTaskCount = 500;
*)



(* ::Section:: *)
(* Methods on epsrCaches *)


getCachedValue[str_String] :=
    Block[ {},
        First[Select[epsrCaches,MatchQ[#,{str,___}]&]][[2]]
    ];
locateCachedValue[str_String] :=
    Block[ {},
        First[Flatten[Position[epsrCaches,{str,___}]]]
    ]



(* ::Section:: *)
(* Prolog *)


indexDates[] :=
    Block[ {absoluteZero, dates, dateSTR,i},
        epsrCaches = Get[FileNameJoin[{epsrResourcesPath,"Resources","epsrCaches.m"}]];
        If[ First@DateList[] != getCachedValue["dateIndexRefYear"],(* if we entered a new year, then we reset the date indexes*)
            absoluteZero = {First@DateList[] - 3, 1, 1};
            dates = Table[DatePlus[absoluteZero, i], {i, 0, 4999}];
            dateSTR = DateString[#, {"Day", "-", "Month", "-", "Year"}] & /@ dates;
            (dateIndex[dateSTR[[#]]] = #) & /@ Range[5000];(*absoluteZero is day 1; convention: % at the end of the day *)
            (revDateIndex[#] = dateSTR[[#]]) & /@ Range[5000];(* this is the reverse date index function *)
            i = locateCachedValue["indexedDatesList"];
            epsrCaches = ReplacePart[epsrCaches,i->{"indexedDatesList",dateSTR}];(* save new list of indexed dates in caches *)
            i = locateCachedValue["dateIndexRefYear"];
            epsrCaches = ReplacePart[epsrCaches,i->{"dateIndexRefYear",First@DateList[]}];(* save new ref year into caches *)
            Put[epsrCaches,FileNameJoin[{epsrResourcesPath,"Resources","epsrCaches.m"}]],
            (*else*)
            dateSTR = getCachedValue["indexedDatesList"];
            (dateIndex[dateSTR[[#]]] = #) & /@ Range[5000];
            (revDateIndex[#] = dateSTR[[#]]) & /@ Range[5000];
            dateIndex[""] = "";(* catches "" exception *)
            revDateIndex[""] = "";
        ]
    ]

indexDates[];
dateIndCorr = 0;

(* translations for display *)
epsrValidActionTranslation["EN"] = {
    "home"->"Home",
    "manage.projs"->"Admin Projects",
    "upload.planning"->"Upload Planning",
    "upload.fb"->"Capture % Values",
    "make.scurves"->"Issue S-Curves",
    "lookup.lasheet"->"Look up LA Sheet",
    "lookup.scurves"->"Look up S-Curves",
    "lookup.planning"->"Look up Planning"
};

(* rules: action-> jsp page *)
(* same action, different pages possible for different privileges *)
epsrValidActions["zeus"] = {
    
    
};

epsrValidActions["pm"] = {
    {"home","MC.jsp"},
    {"upload.planning","upl.jsp"},
    {"upload.fb","fb_capt.jsp"},
    {"make.scurves","epsr.jsp"},
    {"lookup.lasheet","las_lu.jsp"},
    {"lookup.scurves","epsr_lu.jsp"}
};

epsrValidActions["tm"] = {
  {"home", "MC.jsp"},
  {"upload.fb", "fb_capt.jsp"},
  {"lookup.lasheet", "las_lu.jsp"},
  {"lookup.scurves", "epsr_lu.jsp"}
  };

epsrValidActions["*"] =
 {
  {"home", "MC.jsp"}
  };

epsrValidActions["sc"] = {
    {"home","MC.jsp"},
    {"lookup.lasheet","las_lu.jsp"},
    {"lookup.scurves","epsr_lu.jsp"}
};

epsrValidActions["t"] = {
    {"home","MC.jsp"},
     {"lookup.lasheet","las_lu.jsp"}
};

(* headers *)
hdrsDiagnostics = {
        "Top task: ",
        "MS Count: ",
        "Summaries Count: ",
        "Atomic Tasks Count: ",
        "Blank Lines Count: ",
        "Missing Names Count: ",
        "Max Olev: ",
        "Max ddd:",
        "Max s-curves count:",
        "Missing Base Lines Count: ",
        "-p flagged tasks:",
        "Hybrids detected",
        "CLASS",
        "TYPE",
        "LC Limit"
        };

epsrPutAndEncodeFile[obj_,fp_] :=
    Block[ {dir,temp},
        dir = DirectoryName[fp];
        temp = FileNameJoin[{dir,"temp.m"}];
        Put[obj,temp];
        Encode[temp,fp];
        DeleteFile[temp]
    ];

(* default state values *)   
epsrStateDefault =
{
{"UPL.locked",True},
{"UPL.frozen",False},
{"SC.lineLimit",50},
{"SC.olevLimit",7},
{"SC.scLimit",120}
}

(* get the advisor msgs *)
(* ON HOLD *)
(*
epsrAdv["intro"] = Import[FileNameJoin[{epsrSMSHARData, "Resources", "advIntro.txt"}],"TSV"];
epsrAdv["common"] = Import[FileNameJoin[{epsrSMSHARData, "Resources", "advCommon.txt"}],"TSV"];
epsrAdv["spec_1"] = Import[FileNameJoin[{epsrSMSHARData, "Resources", "advSpec_1.m"}],"TSV"];
epsrAdv["spec_2"] = Import[FileNameJoin[{epsrSMSHARData, "Resources", "advSpec_2.m"}],"TSV"];
epsrAdv["spec_3"] = Import[FileNameJoin[{epsrSMSHARData, "Resources", "advSpec_3.m"}],"TSV"];
epsrAdv["spec_4"] = Import[FileNameJoin[{epsrSMSHARData, "Resources", "advSpec_4.m"}],"TSV"];
epsrAdv["spec_5"] = Import[FileNameJoin[{epsrSMSHARData, "Resources", "advSpec_5.m"}],"TSV"];
*)


(* proj types and classes *)
projectClasses = {{"basic",1},{"enterprise",2},{"projectoffice",3}};
taskCountLimits = {{0,50},{1, 100}, {2, 1500}, {3, 5000}};
(maxTaskCount[#[[1]]]=#[[2]])&/@taskCountLimits;

allowedPlanningSystems =
    {
        {"Microsoft Project","MSP"},
        {"Primavera P6","P6"},
        {"OpenProj","OPPR"},
        {"ProTrack","PRTR"},
        {"OmniPlan","OMPL"},
        {"ProjectLibre","PRLIB"},
        {"SmartSheet","SMSH"},
        {"MSP XML","MSPXML"}        
        };
allowedFileExtension = {{"MSP",{"txt"}},{"MSPXML",{"xml"}},{"P6",{"xls","xlsx"}},{"OPPR",{"xml"}},{"PRTR",{"txt"}},{"OMPL",{"txt"}},{"PRLIB",{"xml"}}};

(* maps smsh colnames on epsr col names *)
smshColnamstranslator = {
    "uid"->"uid",
    "At Risk"->"atrisk",
    "Flag"->"epsiFlag",
    "Select"->"select",
    "Task Name"->"nam",
    "Responsible"->"att1",
    "Assigned To"->"att1",
    "WF"->"wf",
    "wf"->"wf",
    "Predecessors"->"succ",
    "Duration"->"dur",
    "Start Date"->"bles",
    "End Date"->"blef",
    "% Complete"->"pct",
    "Tree"->"wbs"
    };

(* SMARTSHEET ACCESS *)   
smshTimeLimit = 40;
smshAccessToken = "***";
smshAccSheetID = "***";

smshMaxlevelforaudit=3;

(* for audit report *)
flagIndex=#[[1]]->#[[2]]&/@Transpose[{{Black,Blue,Green,Yellow,Orange,Red},Range[6]}];
flagIndex=Join[{GrayLevel[0.85]->1},flagIndex];

End[]

EndPackage[]
