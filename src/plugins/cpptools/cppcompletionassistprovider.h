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

#ifndef CPPTOOLS_CPPCOMPLETIONASSISTPROVIDER_H
#define CPPTOOLS_CPPCOMPLETIONASSISTPROVIDER_H

#include "cpptools_global.h"

#include <texteditor/codeassist/assistenums.h>
#include <texteditor/codeassist/completionassistprovider.h>


QT_BEGIN_NAMESPACE
class QTextDocument;
QT_END_NAMESPACE

namespace TextEditor {
class BaseTextEditor;
class AssistInterface;
}

namespace CppTools {

class CPPTOOLS_EXPORT CppCompletionAssistProvider : public TextEditor::CompletionAssistProvider
{
    Q_OBJECT

public:
    bool supportsEditor(Core::Id editorId) const Q_DECL_OVERRIDE;
    int activationCharSequenceLength() const Q_DECL_OVERRIDE;
    bool isActivationCharSequence(const QString &sequence) const Q_DECL_OVERRIDE;
    bool isContinuationChar(const QChar &c) const Q_DECL_OVERRIDE;

    virtual TextEditor::AssistInterface *createAssistInterface(
            const QString &filePath,
            QTextDocument *document, bool isObjCEnabled, int position,
            TextEditor::AssistReason reason) const = 0;

    static int activationSequenceChar(const QChar &ch, const QChar &ch2,
                                      const QChar &ch3, unsigned *kind,
                                      bool wantFunctionCall);
};

} // namespace CppTools

#endif // CPPTOOLS_CPPCOMPLETIONASSISTPROVIDER_H
