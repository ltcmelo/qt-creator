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

#ifndef DIFFEDITORCONSTANTS_H
#define DIFFEDITORCONSTANTS_H

#include <QtGlobal>

namespace DiffEditor {
namespace Constants {

const char DIFF_EDITOR_ID[] = "Diff Editor";
const char DIFF_EDITOR_DISPLAY_NAME[] = QT_TRANSLATE_NOOP("DiffEditor", "Diff Editor");
const char DIFF_EDITOR_MIMETYPE[] = "text/x-patch";
const char G_TOOLS_DIFF[] = "QtCreator.Group.Tools.Options";

const char ICON_SIDE_BY_SIDE_DIFF[]  = ":/diffeditor/images/sidebysidediff.png";
const char ICON_UNIFIED_DIFF[]       = ":/diffeditor/images/unifieddiff.png";
const char ICON_TOP_BAR[]            = ":/diffeditor/images/topbar.png";

const char EXPAND_BRANCHES[] = "Branches: <Expand>";

} // namespace Constants
} // namespace DiffEditor

#endif // DIFFEDITORCONSTANTS_H
