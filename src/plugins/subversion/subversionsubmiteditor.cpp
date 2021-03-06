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


#include "subversionsubmiteditor.h"

#include <coreplugin/idocument.h>
#include <vcsbase/submiteditorwidget.h>
#include <vcsbase/submitfilemodel.h>

using namespace Subversion::Internal;

SubversionSubmitEditor::SubversionSubmitEditor(const VcsBase::VcsBaseSubmitEditorParameters *parameters) :
    VcsBase::VcsBaseSubmitEditor(parameters, new VcsBase::SubmitEditorWidget)
{
    document()->setDisplayName(tr("Subversion Submit"));
    setDescriptionMandatory(false);
}

void SubversionSubmitEditor::setStatusList(const QList<StatusFilePair> &statusOutput)
{
    typedef QList<StatusFilePair>::const_iterator ConstIterator;
    VcsBase::SubmitFileModel *model = new VcsBase::SubmitFileModel(this);

    const ConstIterator cend = statusOutput.constEnd();
    for (ConstIterator it = statusOutput.constBegin(); it != cend; ++it)
        model->addFile(it->second, it->first);
    // Hack to allow completion in "description" field : completion needs a root repository, the
    // checkScriptWorkingDirectory property is fine (at this point it was set by SubversionPlugin)
    setFileModel(model, this->checkScriptWorkingDirectory());
}

/*
QStringList SubversionSubmitEditor::vcsFileListToFileList(const QStringList &rl) const
{
    QStringList files;
    const QStringList::const_iterator cend = rl.constEnd();
    for (QStringList::const_iterator it = rl.constBegin(); it != cend; ++it)
        files.push_back(SubversionSubmitEditor::fileFromStatusLine(*it));
    return files;
}

QString SubversionSubmitEditor::fileFromStatusLine(const QString &statusLine)
{
    enum { filePos = 7 };
    return statusLine.mid(filePos, statusLine.size() - filePos);
}

*/
