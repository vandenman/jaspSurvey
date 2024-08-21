import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspSurvey"
	title		: qsTr("Survey")
	description	: qsTr("This module offers analyses for survey data.")
	version		: "0.19.0"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"
	icon		: "survey.svg"

	Analysis { title: qsTr("Descriptives");		func:	"surveyDescriptives"	}
	Analysis { title: qsTr("Analyses");			func:	"surveyAnalyses"		}

}
