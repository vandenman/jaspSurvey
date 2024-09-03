
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

// All Analysis forms must be built with the From QML item
Form
{
	// info: qsTr("Descriptives allows the user to obtain basic descriptive statistics, histograms and density plots, correlation plots, boxplots, and frequency tables.")
	// infoBottom: "## " + qsTr("References") + "\n"
	// 			+	"- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company." + "\n"
	// 			+	"- Schwarz, G. (1978). Estimating the dimension of a model. *Annals of Statistics, 6*, 461-464." + "\n"
	// 			+	"- Whitlock, M. C., & Schluter, D. (2015). *The analysis of biological data (2nd ed.)*. Greenwood Village, Colorado: Roberts and Company Publishers." + "\n"
	// 			+ "\n---\n"
	// 			+ "## " + qsTr("R Packages") + "\n"
	// 			+	"- ggplot2\n"
	// 			+	"- ggrepel\n"
	// 			+	"- grid\n"
	// 			+	"- stats\n"
	// columns: 1

	Formula
	{
		rhs: "variables"
	}

	VariablesForm
	{
		infoLabel: qsTr("Input")
		AvailableVariablesList	{ name: "allVariablesList"								}
		AssignedVariablesList	{ name: "variables";		title: qsTr("Variables");	info: qsTr("All variables of interest."); allowTypeChange: true }
		AssignedVariablesList	{ name: "weights";			title: qsTr("Weights");		info: qsTr("Can be split by a categorical variable such as experimental condition.") ; singleVariable: true; allowedColumns: ["nominal"]; minLevels: 2; maxLevels: 256 } // without maxLevels entering a continuous variable can freeze/ crash jasp, so we need an arbitrary maximum
		AssignedVariablesList	{ name: "strata";			title: qsTr("Strata");		info: qsTr("Can be split by a categorical variable such as experimental condition.") ; singleVariable: true; allowedColumns: ["nominal"]; minLevels: 2; maxLevels: 256 }
		AssignedVariablesList	{ name: "id";				title: qsTr("Id");			info: qsTr("Can be split by a categorical variable such as experimental condition.") ; singleVariable: true; allowedColumns: ["nominal"]; minLevels: 2; maxLevels: 256 }
	}

	CheckBox
	{
		name	: "descriptivesTableTransposed"
		label	: qsTr("Transpose descriptives table")
		info	: qsTr("Tranposes the main table")
		checked	: false
	}
}
