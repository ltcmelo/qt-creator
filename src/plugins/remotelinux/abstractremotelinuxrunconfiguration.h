/****************************************************************************
**
** Copyright (C) 2014 Canonical Ltd.
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
#ifndef REMOTELINUX_ABSTRACTREMOTELINUXRUNCONFIGURATION_H
#define REMOTELINUX_ABSTRACTREMOTELINUXRUNCONFIGURATION_H

#include "remotelinux_export.h"
#include <projectexplorer/runconfiguration.h>

QT_FORWARD_DECLARE_CLASS(QStringList)

namespace Utils { class Environment; }

namespace RemoteLinux {

class REMOTELINUX_EXPORT AbstractRemoteLinuxRunConfiguration :
        public ProjectExplorer::RunConfiguration
{
    Q_OBJECT
    Q_DISABLE_COPY(AbstractRemoteLinuxRunConfiguration)

public:
    AbstractRemoteLinuxRunConfiguration(ProjectExplorer::Target *parent, Core::Id id);

    ~AbstractRemoteLinuxRunConfiguration();

    virtual QString localExecutableFilePath() const = 0;
    virtual QString remoteExecutableFilePath() const = 0;
    virtual QStringList arguments() const = 0;
    virtual QString workingDirectory() const = 0;
    virtual Utils::Environment environment() const = 0;

    int portsUsedByDebuggers() const;

protected:
    AbstractRemoteLinuxRunConfiguration(ProjectExplorer::Target *parent,
        AbstractRemoteLinuxRunConfiguration *source);
};

} // namespace RemoteLinux

#endif // REMOTELINUX_ABSTRACTREMOTELINUXRUNCONFIGURATION_H
