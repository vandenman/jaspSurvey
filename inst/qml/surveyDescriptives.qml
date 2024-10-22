
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
		AssignedVariablesList	{ name: "weights";			title: weightsOrProbs.value === "weights" ? qsTr("Weights") : qsTr("Probabilities");		info: qsTr("Can be split by a categorical variable such as experimental condition.") ; singleVariable: true; allowedColumns: ["scale"]}
		RadioButtonGroup
		{
			id: weightsOrProbs
			name: "weightsOrProbs"
			radioButtonsOnSameRow: true
			RadioButton { value: "weights";			label: qsTr("Weights");			checked: true	}
			RadioButton { value: "probabilities";	label: qsTr("Probabilities")					}
		}
		AssignedVariablesList	{ name: "strata";		title: qsTr("Strata");								info: qsTr("Can be split by a categorical variable such as experimental condition.") ; singleVariable: true; allowedColumns: ["nominal"]; minLevels: 1; maxLevels: 1024 }
		AssignedVariablesList	{ name: "fpc";			title: qsTr("Finite population correction");		info: qsTr("Can be split by a categorical variable such as experimental condition.") ; singleVariable: true; allowedColumns: ["scale"]; minNumericLevels: 1}
		AssignedVariablesList	{ name: "id";			title: qsTr("Id");									info: qsTr("Can be split by a categorical variable such as experimental condition.") ; singleVariable: true; allowedColumns: ["nominal"]; minLevels: 1; maxLevels: 1024 }

		AssignedVariablesList	{ name: "splitBy";				title: qsTr("Split");			info: qsTr("Can be split by a categorical variables such as experimental condition.") ; singleVariable: false; allowedColumns: ["nominal"]; minLevels: 1; maxLevels: 1024 }
	}

	CheckBox
	{
		name	: "descriptivesTableTransposed"
		label	: qsTr("Transpose descriptives table")
		info	: qsTr("Tranposes the main table")
		checked	: false
	}

	Section
	{
		title: qsTr("Statistics")

		Group
		{
			title: qsTr("Outcomes")
			infoLabel: qsTr("Central Tendency (only for continuous variables)")

			CheckBox { name: "mean";		label: qsTr("Mean");		info: qsTr("Weighted estimation of the design mean for the selected variables.")	 ; checked: true	}
			CheckBox { name: "total";		label: qsTr("Total");		info: qsTr("Weighted estimation of the design total for the selected variables.")						}
			CheckBox { name: "var";			label: qsTr("Variance");	info: qsTr("Weighted estimation of the design variance for the selected variables.")					}
		}

		Group
		{
			title: qsTr("Uncertainty")
			CheckBox
			{
				name: "ci"; label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "ciLevel" }
			}
			CheckBox { name: "se";		label: qsTr("Standard error");				info: qsTr("Weighted estimate of uncerestimation of the design mean for the selected variables.")						}
			CheckBox { name: "cv";		label: qsTr("Coefficient of variation");	info: qsTr("Weighted estimate of uncerestimation of the design mean for the selected variables.")						}
		}
	}

	Section
	{
		title: qsTr("Basic plots")
		columns: 2

		Group
		{
			Row
			{
				spacing: jaspTheme.columnGroupSpacing
				CheckBox
				{
					name: "distributionPlots";	label: qsTr("Distribution plots");	id:	distributionPlots
					info: qsTr("For continuous variables, displays a histogram and the fit of a nonparametric density estimator. For nominal and ordinal variables, displays a frequency distribution.")
				}
			}
		}
	}
}
