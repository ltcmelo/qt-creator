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

#ifndef QMLDESIGNER_ITEMLIBRARYSECTION_H
#define QMLDESIGNER_ITEMLIBRARYSECTION_H

#include "itemlibrarysectionmodel.h"

namespace QmlDesigner {

class ItemLibrarySection: public QObject {

    Q_OBJECT

    Q_PROPERTY(QObject* sectionEntries READ sectionEntries NOTIFY sectionEntriesChanged FINAL)
    Q_PROPERTY(QString sectionName READ sectionName FINAL)
    Q_PROPERTY(bool sectionVisible READ isVisible NOTIFY visibilityChanged FINAL)
    Q_PROPERTY(bool sectionExpanded READ sectionExpanded FINAL)

public:
    ItemLibrarySection(const QString &sectionName, QObject *parent = 0);

    QString sectionName() const;
    bool sectionExpanded() const;
    QString sortingName() const;

    void addSectionEntry(ItemLibraryItem *sectionEntry);
    QObject *sectionEntries();

    bool updateSectionVisibility(const QString &searchText, bool *changed);

    bool setVisible(bool isVisible);
    bool isVisible() const;

    void sortItems();

signals:
    void sectionEntriesChanged();
    void visibilityChanged();

private:
    ItemLibrarySectionModel m_sectionEntries;
    QString m_name;
    bool m_sectionExpanded;
    bool m_isVisible;
};

} // namespace QmlDesigner

#endif // QMLDESIGNER_ITEMLIBRARYSECTION_H
