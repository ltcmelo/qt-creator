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

#ifndef DIFFEDITORRELOADER_H
#define DIFFEDITORRELOADER_H

#include "diffeditor_global.h"

#include <QObject>

namespace DiffEditor {

class DiffEditorController;

class DIFFEDITOR_EXPORT DiffEditorReloader : public QObject
{
    Q_OBJECT
public:
    DiffEditorReloader();
    ~DiffEditorReloader();

    bool isReloading() const;
    DiffEditorController *controller() const;

public slots:
    void requestReload();

protected:
    // reloadFinished() should be called
    // inside reload() (for synchronous reload)
    // or later (for asynchronous reload)
    virtual void reload() = 0;
    void setController(DiffEditorController *controller);

protected slots:
    void reloadFinished();

private:
    DiffEditorController *m_controller;
    bool m_reloading;

    friend class DiffEditorController;
};

} // namespace DiffEditor

#endif // DIFFEDITORRELOADER_H
