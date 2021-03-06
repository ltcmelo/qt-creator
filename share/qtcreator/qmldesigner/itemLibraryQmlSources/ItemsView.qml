/****************************************************************************
**
** Copyright (C) 2014 Digia Plc and/or its subsidiary(-ies).
** Contact: http://www.qt-project.org/legal
**
** This file is part of Qt Creator.
**
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Digia.  For licensing terms and
** conditions see http://www.qt.io/licensing.  For further information
** use the contact form at http://www.qt.io/contact-us.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file.  Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Digia gives you certain additional
** rights.  These rights are described in the Digia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
****************************************************************************/

import QtQuick 2.1
import QtQuick.Controls 1.1
import QtQuick.Controls.Styles 1.0
import "../common"

import QtQuick.Layouts 1.0
import "../propertyEditorQmlSources/HelperWidgets"


/* The view displaying the item grid.

The following Qml context properties have to be set:
- listmodel itemLibraryModel
- int itemLibraryIconWidth
- int itemLibraryIconHeight

itemLibraryModel has to have the following structure:

ListModel {
ListElement {
int sectionLibId
string sectionName
list sectionEntries: [
ListElement {
int itemLibId
string itemName
pixmap itemPixmap
},
...
]
}
...
}
*/


ScrollView {
    id: itemsView

    Item {
        id: styleConstants
        property color backgroundColor: "#4f4f4f"
        property color lighterBackgroundColor: "#5f5f5f"

        property int textWidth: 55
        property int textHeight: 26

        property int cellHorizontalMargin: 5
        property int cellVerticalSpacing: 3
        property int cellVerticalMargin: 7

        // the following depend on the actual shape of the item delegate
        property int cellWidth: textWidth + 2 * cellHorizontalMargin
        property int cellHeight: itemLibraryIconHeight + textHeight +
        2 * cellVerticalMargin + cellVerticalSpacing
    }

    Rectangle {
        id: background
        anchors.fill: parent
        color: styleConstants.backgroundColor
    }

    style: DesignerScrollViewStyle {

    }

    Flickable {
        contentHeight: column.height
        Column {
            id: column
            Repeater {
                model: itemLibraryModel  // to be set in Qml context
                delegate: Section {
                    width: itemsView.viewport.width
                    caption: sectionName // to be set by model
                    visible: sectionVisible
                    topPadding: 2
                    leftPadding: 2
                    rightPadding: 1
                    Grid {
                        id: itemGrid

                        columns: parent.width / styleConstants.cellWidth
                        property int flexibleWidth: (parent.width - styleConstants.cellWidth * columns) / columns

                        Repeater {
                            model: sectionEntries
                            delegate: ItemDelegate {
                                visible: itemVisible
                                width: styleConstants.cellWidth + itemGrid.flexibleWidth
                                height: styleConstants.cellHeight
                            }
                        }
                        move: Transition {
                            NumberAnimation {
                                properties: "x, y";
                                easing.type: Easing.OutQuart
                                duration: 80
                            }
                        }
                    }
                }
            }
        }
    }
}
