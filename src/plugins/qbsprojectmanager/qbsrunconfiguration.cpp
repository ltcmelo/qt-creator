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

#include "qbsrunconfiguration.h"

#include "qbsdeployconfigurationfactory.h"
#include "qbsinstallstep.h"
#include "qbsproject.h"

#include <coreplugin/coreconstants.h>
#include <projectexplorer/buildstep.h>
#include <projectexplorer/buildsteplist.h>
#include <projectexplorer/deployconfiguration.h>
#include <projectexplorer/localapplicationruncontrol.h>
#include <projectexplorer/localenvironmentaspect.h>
#include <projectexplorer/target.h>
#include <utils/qtcprocess.h>
#include <utils/pathchooser.h>
#include <utils/detailswidget.h>
#include <utils/stringutils.h>
#include <utils/persistentsettings.h>
#include <qtsupport/qtoutputformatter.h>
#include <qtsupport/qtsupportconstants.h>
#include <qtsupport/qtkitinformation.h>
#include <utils/hostosinfo.h>

#include "api/runenvironment.h"

#include <QFileInfo>
#include <QFormLayout>
#include <QLabel>
#include <QLineEdit>
#include <QCheckBox>
#include <QToolButton>
#include <QComboBox>
#include <QDir>

using namespace ProjectExplorer;

namespace QbsProjectManager {
namespace Internal {

const char QBS_RC_PREFIX[] = "Qbs.RunConfiguration:";
const char COMMAND_LINE_ARGUMENTS_KEY[] = "Qbs.RunConfiguration.CommandLineArguments";
const char USE_TERMINAL_KEY[] = "Qbs.RunConfiguration.UseTerminal";
const char USER_WORKING_DIRECTORY_KEY[] = "Qbs.RunConfiguration.UserWorkingDirectory";

static QString rcNameSeparator() { return QLatin1String("---Qbs.RC.NameSeparator---"); }

static Core::Id idFromProduct(const QbsProject *project, const qbs::ProductData &product)
{
    QString id = QLatin1String(QBS_RC_PREFIX);
    id.append(QbsProject::uniqueProductName(product)).append(rcNameSeparator())
            .append(QbsProject::productDisplayName(project->qbsProject(), product));
    return Core::Id::fromString(id);
}

static QString uniqueProductNameFromId(Core::Id id)
{
    const QString suffix = id.suffixAfter(QBS_RC_PREFIX);
    return suffix.left(suffix.indexOf(rcNameSeparator()));
}

static QString productDisplayNameFromId(Core::Id id)
{
    const QString suffix = id.suffixAfter(QBS_RC_PREFIX);
    const int sepPos = suffix.indexOf(rcNameSeparator());
    if (sepPos == -1)
        return suffix;
    return suffix.mid(sepPos + rcNameSeparator().count());
}

const qbs::ProductData findProduct(const qbs::ProjectData &pro, const QString &uniqeName)
{
    foreach (const qbs::ProductData &product, pro.allProducts()) {
        if (QbsProject::uniqueProductName(product) == uniqeName)
            return product;
    }
    return qbs::ProductData();
}

// --------------------------------------------------------------------
// QbsRunConfiguration:
// --------------------------------------------------------------------

QbsRunConfiguration::QbsRunConfiguration(Target *parent, Core::Id id) :
    LocalApplicationRunConfiguration(parent, id),
    m_uniqueProductName(uniqueProductNameFromId(id)),
    m_runMode(ApplicationLauncher::Gui),
    m_currentInstallStep(0),
    m_currentBuildStepList(0)
{
    addExtraAspect(new LocalEnvironmentAspect(this));

    m_runModeForced = false;
    m_runMode = isConsoleApplication() ? ApplicationLauncher::Console : ApplicationLauncher::Gui;

    ctor();
}

QbsRunConfiguration::QbsRunConfiguration(Target *parent, QbsRunConfiguration *source) :
    LocalApplicationRunConfiguration(parent, source),
    m_uniqueProductName(source->m_uniqueProductName),
    m_commandLineArguments(source->m_commandLineArguments),
    m_runMode(source->m_runMode),
    m_runModeForced(source->m_runModeForced),
    m_userWorkingDirectory(source->m_userWorkingDirectory),
    m_currentInstallStep(0), // no need to copy this, we will get if from the DC anyway.
    m_currentBuildStepList(0) // ditto
{
    ctor();
}

bool QbsRunConfiguration::isEnabled() const
{
    QbsProject *project = static_cast<QbsProject *>(target()->project());
    return !project->isParsing() && project->hasParseResult();
}

QString QbsRunConfiguration::disabledReason() const
{
    QbsProject *project = static_cast<QbsProject *>(target()->project());
    if (project->isParsing())
        return tr("The .qbs files are currently being parsed.");

    if (!project->hasParseResult())
        return tr("Parsing of .qbs files has failed.");
    return QString();
}

void QbsRunConfiguration::ctor()
{
    setDefaultDisplayName(defaultDisplayName());

    QbsProject *project = static_cast<QbsProject *>(target()->project());
    connect(project, &QbsProject::projectParsingStarted, this, &RunConfiguration::enabledChanged);
    connect(project, &QbsProject::projectParsingDone, this, [this](bool success) {
        if (success && !m_runModeForced)
            m_runMode = isConsoleApplication() ? ApplicationLauncher::Console
                                               : ApplicationLauncher::Gui;
        emit enabledChanged();
    });

    connect(target(), &Target::activeDeployConfigurationChanged,
            this, &QbsRunConfiguration::installStepChanged);
    installStepChanged();
}

QWidget *QbsRunConfiguration::createConfigurationWidget()
{
    return new QbsRunConfigurationWidget(this, 0);
}

QVariantMap QbsRunConfiguration::toMap() const
{
    QVariantMap map(LocalApplicationRunConfiguration::toMap());
    map.insert(QLatin1String(COMMAND_LINE_ARGUMENTS_KEY), m_commandLineArguments);
    if (m_runModeForced)
        map.insert(QLatin1String(USE_TERMINAL_KEY), m_runMode == ApplicationLauncher::Console);
    map.insert(QLatin1String(USER_WORKING_DIRECTORY_KEY), m_userWorkingDirectory);
    return map;
}

bool QbsRunConfiguration::fromMap(const QVariantMap &map)
{
    m_commandLineArguments = map.value(QLatin1String(COMMAND_LINE_ARGUMENTS_KEY)).toString();
    if (map.contains(QLatin1String(USE_TERMINAL_KEY))) {
        m_runMode = map.value(QLatin1String(USE_TERMINAL_KEY), false).toBool() ?
                    ApplicationLauncher::Console : ApplicationLauncher::Gui;
        m_runModeForced = true;
    }

    m_userWorkingDirectory = map.value(QLatin1String(USER_WORKING_DIRECTORY_KEY)).toString();

    return RunConfiguration::fromMap(map);
}

void QbsRunConfiguration::installStepChanged()
{
    if (m_currentInstallStep)
        disconnect(m_currentInstallStep, SIGNAL(changed()), this, SIGNAL(targetInformationChanged()));
    if (m_currentBuildStepList) {
        disconnect(m_currentBuildStepList, SIGNAL(stepInserted(int)), this, SLOT(installStepChanged()));
        disconnect(m_currentBuildStepList, SIGNAL(stepRemoved(int)), this, SLOT(installStepChanged()));
        disconnect(m_currentBuildStepList, SIGNAL(stepMoved(int,int)), this, SLOT(installStepChanged()));
    }

    QbsDeployConfiguration *activeDc = qobject_cast<QbsDeployConfiguration *>(target()->activeDeployConfiguration());
    m_currentBuildStepList = activeDc ? activeDc->stepList() : 0;
    m_currentInstallStep = activeDc ? activeDc->qbsInstallStep() : 0;

    if (m_currentInstallStep)
        connect(m_currentInstallStep, SIGNAL(changed()), this, SIGNAL(targetInformationChanged()));

    if (m_currentBuildStepList) {
        connect(m_currentBuildStepList, SIGNAL(stepInserted(int)), this, SLOT(installStepChanged()));
        connect(m_currentBuildStepList, SIGNAL(aboutToRemoveStep(int)), this,
                SLOT(installStepToBeRemoved(int)));
        connect(m_currentBuildStepList, SIGNAL(stepRemoved(int)), this, SLOT(installStepChanged()));
        connect(m_currentBuildStepList, SIGNAL(stepMoved(int,int)), this, SLOT(installStepChanged()));
    }

    emit targetInformationChanged();
}

void QbsRunConfiguration::installStepToBeRemoved(int pos)
{
    QTC_ASSERT(m_currentBuildStepList, return);
    // TODO: Our logic is rather broken. Users can create as many qbs install steps as they want,
    // but we ignore all but the first one.
    if (m_currentBuildStepList->steps().at(pos) != m_currentInstallStep)
        return;
    disconnect(m_currentInstallStep, SIGNAL(changed()), this, SIGNAL(targetInformationChanged()));
    m_currentInstallStep = 0;
}

QString QbsRunConfiguration::executable() const
{
    QbsProject *pro = static_cast<QbsProject *>(target()->project());
    const qbs::ProductData product = findProduct(pro->qbsProjectData(), m_uniqueProductName);

    if (!product.isValid() || !pro->qbsProject().isValid())
        return QString();

    return pro->qbsProject().targetExecutable(product, installOptions());
}

ApplicationLauncher::Mode QbsRunConfiguration::runMode() const
{
    return m_runMode;
}

bool QbsRunConfiguration::isConsoleApplication() const
{
    QbsProject *pro = static_cast<QbsProject *>(target()->project());
    const qbs::ProductData product = findProduct(pro->qbsProjectData(), m_uniqueProductName);
    return product.properties().value(QLatin1String("consoleApplication"), false).toBool();
}

QString QbsRunConfiguration::workingDirectory() const
{
    EnvironmentAspect *aspect = extraAspect<EnvironmentAspect>();
    QTC_ASSERT(aspect, return baseWorkingDirectory());
    return QDir::cleanPath(aspect->environment().expandVariables(
                macroExpander()->expand(baseWorkingDirectory())));
}

QString QbsRunConfiguration::baseWorkingDirectory() const
{
    // if the user overrode us, then return his working directory
    if (!m_userWorkingDirectory.isEmpty())
        return m_userWorkingDirectory;

    // else what the pro file reader tells us
    const QString exe = executable();
    if (!exe.isEmpty())
        return QFileInfo(executable()).absolutePath();
    return QString();
}

QString QbsRunConfiguration::commandLineArguments() const
{
    return macroExpander()->expandProcessArgs(m_commandLineArguments);
}

QString QbsRunConfiguration::rawCommandLineArguments() const
{
    return m_commandLineArguments;
}

void QbsRunConfiguration::setBaseWorkingDirectory(const QString &wd)
{
    const QString &oldWorkingDirectory = workingDirectory();

    m_userWorkingDirectory = wd;

    const QString &newWorkingDirectory = workingDirectory();
    if (oldWorkingDirectory != newWorkingDirectory)
        emit baseWorkingDirectoryChanged(newWorkingDirectory);
}

void QbsRunConfiguration::setCommandLineArguments(const QString &argumentsString)
{
    m_commandLineArguments = argumentsString;
    emit commandLineArgumentsChanged(argumentsString);
}

void QbsRunConfiguration::setRunMode(ApplicationLauncher::Mode runMode)
{
    if (m_runMode == runMode)
        return;

    m_runModeForced = true;
    m_runMode = runMode;
    emit runModeChanged(runMode);
}

void QbsRunConfiguration::addToBaseEnvironment(Utils::Environment &env) const
{
    QbsProject *project = static_cast<QbsProject *>(target()->project());
    if (project) {
        const qbs::ProductData product = findProduct(project->qbsProjectData(), m_uniqueProductName);
        if (product.isValid()) {
            QProcessEnvironment procEnv = env.toProcessEnvironment();
            procEnv.insert(QLatin1String("QBS_INSTALL_ROOT"), installRoot());
            procEnv.insert(QLatin1String("QBS_RUN_FILE_PATH"), executable());
            qbs::RunEnvironment qbsRunEnv = project->qbsProject().getRunEnvironment(product, installOptions(),
                    procEnv, QbsManager::settings());
            procEnv = qbsRunEnv.runEnvironment();
            if (!procEnv.isEmpty()) {
                env = Utils::Environment();
                foreach (const QString &key, procEnv.keys())
                    env.set(key, procEnv.value(key));
            }
        }
    }

    QtSupport::BaseQtVersion *qtVersion = QtSupport::QtKitInformation::qtVersion(target()->kit());
    if (qtVersion)
        env.prependOrSetLibrarySearchPath(qtVersion->qmakeProperty("QT_INSTALL_LIBS"));
}

QString QbsRunConfiguration::uniqueProductName() const
{
    return m_uniqueProductName;
}

QString QbsRunConfiguration::defaultDisplayName()
{
    QString defaultName = productDisplayNameFromId(id());
    if (defaultName.isEmpty())
        defaultName = tr("Qbs Run Configuration");
    return defaultName;
}

qbs::InstallOptions QbsRunConfiguration::installOptions() const
{
    if (m_currentInstallStep)
        return m_currentInstallStep->installOptions();
    return qbs::InstallOptions();
}

QString QbsRunConfiguration::installRoot() const
{
    if (m_currentInstallStep)
        return m_currentInstallStep->absoluteInstallRoot();
    return QString();
}

Utils::OutputFormatter *QbsRunConfiguration::createOutputFormatter() const
{
    return new QtSupport::QtOutputFormatter(target()->project());
}

// --------------------------------------------------------------------
// QbsRunConfigurationWidget:
// --------------------------------------------------------------------

QbsRunConfigurationWidget::QbsRunConfigurationWidget(QbsRunConfiguration *rc, QWidget *parent)
    : QWidget(parent),
    m_rc(rc),
    m_ignoreChange(false),
    m_isShown(false)
{
    QVBoxLayout *vboxTopLayout = new QVBoxLayout(this);
    vboxTopLayout->setMargin(0);

    QHBoxLayout *hl = new QHBoxLayout();
    hl->addStretch();
    m_disabledIcon = new QLabel(this);
    m_disabledIcon->setPixmap(QPixmap(QLatin1String(Core::Constants::ICON_WARNING)));
    hl->addWidget(m_disabledIcon);
    m_disabledReason = new QLabel(this);
    m_disabledReason->setVisible(false);
    hl->addWidget(m_disabledReason);
    hl->addStretch();
    vboxTopLayout->addLayout(hl);

    m_detailsContainer = new Utils::DetailsWidget(this);
    m_detailsContainer->setState(Utils::DetailsWidget::NoSummary);
    vboxTopLayout->addWidget(m_detailsContainer);
    QWidget *detailsWidget = new QWidget(m_detailsContainer);
    m_detailsContainer->setWidget(detailsWidget);
    QFormLayout *toplayout = new QFormLayout(detailsWidget);
    toplayout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    toplayout->setMargin(0);

    m_executableLineEdit = new QLineEdit(this);
    m_executableLineEdit->setEnabled(false);
    m_executableLineEdit->setPlaceholderText(tr("<unknown>"));
    toplayout->addRow(tr("Executable:"), m_executableLineEdit);

    QLabel *argumentsLabel = new QLabel(tr("Arguments:"), this);
    m_argumentsLineEdit = new QLineEdit(m_rc->rawCommandLineArguments(), this);
    argumentsLabel->setBuddy(m_argumentsLineEdit);
    toplayout->addRow(argumentsLabel, m_argumentsLineEdit);

    m_workingDirectoryEdit = new Utils::PathChooser(this);
    m_workingDirectoryEdit->setHistoryCompleter(QLatin1String("Qbs.WorkingDir.History"));
    m_workingDirectoryEdit->setExpectedKind(Utils::PathChooser::Directory);
    EnvironmentAspect *aspect = m_rc->extraAspect<EnvironmentAspect>();
    if (aspect) {
        connect(aspect, SIGNAL(environmentChanged()), this, SLOT(environmentWasChanged()));
        environmentWasChanged();
    }
    m_workingDirectoryEdit->setPromptDialogTitle(tr("Select Working Directory"));

    QToolButton *resetButton = new QToolButton(this);
    resetButton->setToolTip(tr("Reset to default"));
    resetButton->setIcon(QIcon(QLatin1String(Core::Constants::ICON_RESET)));

    QHBoxLayout *boxlayout = new QHBoxLayout();
    boxlayout->setMargin(0);
    boxlayout->addWidget(m_workingDirectoryEdit);
    boxlayout->addWidget(resetButton);
    toplayout->addRow(tr("Working directory:"), boxlayout);

    QHBoxLayout *innerBox = new QHBoxLayout();
    m_useTerminalCheck = new QCheckBox(tr("Run in terminal"), this);
    innerBox->addWidget(m_useTerminalCheck);

    innerBox->addStretch();
    toplayout->addRow(QString(), innerBox);

    runConfigurationEnabledChange();

    connect(m_workingDirectoryEdit, SIGNAL(changed(QString)),
            this, SLOT(workDirectoryEdited()));

    connect(resetButton, SIGNAL(clicked()),
            this, SLOT(workingDirectoryWasReset()));

    connect(m_argumentsLineEdit, SIGNAL(textEdited(QString)),
            this, SLOT(argumentsEdited(QString)));
    connect(m_useTerminalCheck, SIGNAL(toggled(bool)),
            this, SLOT(termToggled(bool)));

    connect(m_rc, SIGNAL(baseWorkingDirectoryChanged(QString)),
            this, SLOT(workingDirectoryChanged(QString)));

    connect(m_rc, SIGNAL(commandLineArgumentsChanged(QString)),
            this, SLOT(commandLineArgumentsChanged(QString)));
    connect(m_rc, SIGNAL(runModeChanged(ProjectExplorer::ApplicationLauncher::Mode)),
            this, SLOT(runModeChanged(ProjectExplorer::ApplicationLauncher::Mode)));
    connect(m_rc, SIGNAL(targetInformationChanged()),
            this, SLOT(targetInformationHasChanged()), Qt::QueuedConnection);

    connect(m_rc, SIGNAL(enabledChanged()),
            this, SLOT(runConfigurationEnabledChange()));
}

void QbsRunConfigurationWidget::environmentWasChanged()
{
    EnvironmentAspect *aspect = m_rc->extraAspect<EnvironmentAspect>();
    QTC_ASSERT(aspect, return);
    m_workingDirectoryEdit->setEnvironment(aspect->environment());
}

void QbsRunConfigurationWidget::runConfigurationEnabledChange()
{
    bool enabled = m_rc->isEnabled();
    m_disabledIcon->setVisible(!enabled);
    m_disabledReason->setVisible(!enabled);
    m_disabledReason->setText(m_rc->disabledReason());

    m_useTerminalCheck->setChecked(m_rc->runMode() == ApplicationLauncher::Console);
    targetInformationHasChanged();
}

void QbsRunConfigurationWidget::workDirectoryEdited()
{
    if (m_ignoreChange)
        return;
    m_ignoreChange = true;
    m_rc->setBaseWorkingDirectory(m_workingDirectoryEdit->rawPath());
    m_ignoreChange = false;
}

void QbsRunConfigurationWidget::workingDirectoryWasReset()
{
    // This emits a signal connected to workingDirectoryChanged()
    // that sets the m_workingDirectoryEdit
    m_rc->setBaseWorkingDirectory(QString());
}

void QbsRunConfigurationWidget::argumentsEdited(const QString &args)
{
    m_ignoreChange = true;
    m_rc->setCommandLineArguments(args);
    m_ignoreChange = false;
}

void QbsRunConfigurationWidget::termToggled(bool on)
{
    m_ignoreChange = true;
    m_rc->setRunMode(on ? ApplicationLauncher::Console : ApplicationLauncher::Gui);
    m_ignoreChange = false;
}

void QbsRunConfigurationWidget::targetInformationHasChanged()
{
    m_ignoreChange = true;
    m_executableLineEdit->setText(m_rc->executable());

    m_workingDirectoryEdit->setPath(m_rc->baseWorkingDirectory());
    m_workingDirectoryEdit->setBaseFileName(m_rc->target()->project()->projectDirectory());
    m_ignoreChange = false;
}

void QbsRunConfigurationWidget::workingDirectoryChanged(const QString &workingDirectory)
{
    if (!m_ignoreChange)
        m_workingDirectoryEdit->setPath(workingDirectory);
}

void QbsRunConfigurationWidget::commandLineArgumentsChanged(const QString &args)
{
    if (m_ignoreChange)
        return;
    m_argumentsLineEdit->setText(args);
}

void QbsRunConfigurationWidget::runModeChanged(ApplicationLauncher::Mode runMode)
{
    if (!m_ignoreChange)
        m_useTerminalCheck->setChecked(runMode == ApplicationLauncher::Console);
}

// --------------------------------------------------------------------
// QbsRunConfigurationFactory:
// --------------------------------------------------------------------

QbsRunConfigurationFactory::QbsRunConfigurationFactory(QObject *parent) :
    IRunConfigurationFactory(parent)
{
    setObjectName(QLatin1String("QbsRunConfigurationFactory"));
}

QbsRunConfigurationFactory::~QbsRunConfigurationFactory()
{ }

bool QbsRunConfigurationFactory::canCreate(Target *parent, Core::Id id) const
{
    if (!canHandle(parent))
        return false;

    QbsProject *project = static_cast<QbsProject *>(parent->project());
    return findProduct(project->qbsProjectData(), uniqueProductNameFromId(id)).isValid();
}

RunConfiguration *QbsRunConfigurationFactory::doCreate(Target *parent, Core::Id id)
{
    return new QbsRunConfiguration(parent, id);
}

bool QbsRunConfigurationFactory::canRestore(Target *parent, const QVariantMap &map) const
{
    if (!canHandle(parent))
        return false;
    return idFromMap(map).toString().startsWith(QLatin1String(QBS_RC_PREFIX));
}

RunConfiguration *QbsRunConfigurationFactory::doRestore(Target *parent, const QVariantMap &map)
{
    return new QbsRunConfiguration(parent, idFromMap(map));
}

bool QbsRunConfigurationFactory::canClone(Target *parent, RunConfiguration *source) const
{
    return canCreate(parent, source->id());
}

RunConfiguration *QbsRunConfigurationFactory::clone(Target *parent, RunConfiguration *source)
{
    if (!canClone(parent, source))
        return 0;
    QbsRunConfiguration *old = static_cast<QbsRunConfiguration *>(source);
    return new QbsRunConfiguration(parent, old);
}

QList<Core::Id> QbsRunConfigurationFactory::availableCreationIds(Target *parent, CreationMode mode) const
{
    Q_UNUSED(mode)
    QList<Core::Id> result;
    if (!canHandle(parent))
        return result;

    QbsProject *project = static_cast<QbsProject *>(parent->project());
    if (!project || !project->qbsProject().isValid())
        return result;

    foreach (const qbs::ProductData &product, project->qbsProjectData().allProducts()) {
        if (product.isRunnable() && product.isEnabled())
            result << idFromProduct(project, product);
    }

    return result;
}

QString QbsRunConfigurationFactory::displayNameForId(Core::Id id) const
{
    return productDisplayNameFromId(id);
}

bool QbsRunConfigurationFactory::canHandle(Target *t) const
{
    if (!t->project()->supportsKit(t->kit()))
        return false;
    if (!qobject_cast<QbsProject *>(t->project()))
        return false;
    Core::Id devType = DeviceTypeKitInformation::deviceTypeId(t->kit());
    return devType == Constants::DESKTOP_DEVICE_TYPE;
}

} // namespace Internal
} // namespace QbsProjectManager
