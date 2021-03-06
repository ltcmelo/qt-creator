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

#include "nodeinstancesignalspy.h"
#include "objectnodeinstance.h"

#include <QMetaProperty>
#include <QMetaObject>
#include <QDebug>
#include <QSharedPointer>
#include <private/qqmlmetatype_p.h>
#include <QQmlProperty>

namespace QmlDesigner {
namespace Internal {

NodeInstanceSignalSpy::NodeInstanceSignalSpy() :
    QObject()
{
    blockSignals(true);
}

void NodeInstanceSignalSpy::setObjectNodeInstance(const ObjectNodeInstance::Pointer &nodeInstance)
{
    methodeOffset = QObject::staticMetaObject.methodCount() + 1;
    registerObject(nodeInstance->object());
    m_objectNodeInstance = nodeInstance;

}

void NodeInstanceSignalSpy::registerObject(QObject *spiedObject)
{
    if (m_registeredObjectList.contains(spiedObject)) // prevent cycles
        return;

    m_registeredObjectList.append(spiedObject);
    for (int index = QObject::staticMetaObject.propertyOffset();
         index < spiedObject->metaObject()->propertyCount();
         index++) {
        QMetaProperty metaProperty = spiedObject->metaObject()->property(index);

        registerProperty(metaProperty, spiedObject);
        registerChildObject(metaProperty, spiedObject);
    }
}

void NodeInstanceSignalSpy::registerProperty(const QMetaProperty &metaProperty, QObject *spiedObject, const PropertyName &propertyPrefix)
{
    if (metaProperty.isReadable()
            && metaProperty.isWritable()
            && !QQmlMetaType::isQObject(metaProperty.userType())
            && metaProperty.hasNotifySignal()) {
        QMetaMethod metaMethod = metaProperty.notifySignal();
        QMetaObject::connect(spiedObject, metaMethod.methodIndex(), this, methodeOffset, Qt::DirectConnection);

        m_indexPropertyHash.insert(methodeOffset, propertyPrefix + PropertyName(metaProperty.name()));

        registerValueType(metaProperty, spiedObject, propertyPrefix);

        methodeOffset++;
    }
}

void NodeInstanceSignalSpy::registerValueType(const QMetaProperty &metaProperty, QObject *spiedObject, const PropertyName &propertyPrefix)
{
    if (QQmlValueTypeFactory::valueType(metaProperty.userType())) {
        QQmlValueType *valueType = QQmlValueTypeFactory::valueType(metaProperty.userType());
        valueType->setValue(metaProperty.read(spiedObject));
        for (int index = QObject::staticMetaObject.propertyOffset();
             index < valueType->metaObject()->propertyCount();
             index++) {
            QMetaProperty valueTypeMetaProperty = valueType->metaObject()->property(index);

            m_indexPropertyHash.insert(methodeOffset, propertyPrefix + PropertyName(metaProperty.name()) + "." + valueTypeMetaProperty.name());
        }
    }
}

void NodeInstanceSignalSpy::registerChildObject(const QMetaProperty &metaProperty, QObject *spiedObject)
{
    if (metaProperty.isReadable()
            && !metaProperty.isWritable()
            && QQmlMetaType::isQObject(metaProperty.userType())
            && QLatin1String(metaProperty.name()) != "parent") {
        QObject *childObject = QQmlMetaType::toQObject(metaProperty.read(spiedObject));

        if (childObject) {
            for (int index = QObject::staticMetaObject.propertyOffset();
                 index < childObject->metaObject()->propertyCount();
                 index++) {
                QMetaProperty childMetaProperty = childObject->metaObject()->property(index);
                registerProperty(childMetaProperty, childObject, PropertyName(metaProperty.name()) + '.');
            }
        }
    }
}

int NodeInstanceSignalSpy::qt_metacall(QMetaObject::Call call, int methodId, void **a)
{
    if (call == QMetaObject::InvokeMetaMethod && methodId > QObject::staticMetaObject.methodCount()) {
        ObjectNodeInstance::Pointer nodeInstance = m_objectNodeInstance.toStrongRef();

        if (nodeInstance && nodeInstance->nodeInstanceServer() && nodeInstance->isValid()) {
            foreach (const PropertyName &propertyName, m_indexPropertyHash.values(methodId))
                nodeInstance->nodeInstanceServer()->notifyPropertyChange(nodeInstance->instanceId(), propertyName);
        }

    }

    return QObject::qt_metacall(call, methodId, a);
}

} // namespace Internal
} // namespace QmlDesigner
