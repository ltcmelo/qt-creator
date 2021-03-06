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

#ifndef WINDOWSUPPORT_H
#define WINDOWSUPPORT_H

#include "icontext.h"

#include <QObject>

QT_BEGIN_NAMESPACE
class QAction;
class QMenu;
class QWidget;
QT_END_NAMESPACE

namespace Core {
namespace Internal {

class WindowList
{
public:
    static void addWindow(QWidget *window);
    static void removeWindow(QWidget *window);
    static void setActiveWindow(QWidget *window);

private:
    static void activateWindow(QAction *action);
    static void updateTitle(QWidget *window);

    static QMenu *m_dockMenu;
    static QList<QWidget *> m_windows;
    static QList<QAction *> m_windowActions;
    static QList<Id> m_windowActionIds;
};

class WindowSupport : public QObject
{
    Q_OBJECT
public:
    WindowSupport(QWidget *window, const Context &context);
    ~WindowSupport();

    void setCloseActionEnabled(bool enabled);

protected:
    bool eventFilter(QObject *obj, QEvent *event);

private slots:
    void toggleFullScreen();
    void updateFullScreenAction();

private:
    QWidget *m_window;
    IContext *m_contextObject;
    QAction *m_minimizeAction;
    QAction *m_zoomAction;
    QAction *m_closeAction;
    QAction *m_toggleFullScreenAction;
};

} // Internal
} // Core

#endif // WINDOWSUPPORT_H
