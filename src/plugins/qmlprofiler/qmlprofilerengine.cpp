#include "qmlprofilerengine.h"

#include "qmlprofilerplugin.h"
#include "qmlprofilertool.h"

#include <projectexplorer/applicationrunconfiguration.h>

#include <private/qdeclarativedebugclient_p.h>

#include "timelineview.h"
#include "tracewindow.h"

#include <QDebug>

#include "canvas/qdeclarativecanvas_p.h"
#include "canvas/qdeclarativecontext2d_p.h"
#include "canvas/qdeclarativetiledcanvas_p.h"

#include <utils/environment.h>
#include <QProcess>
#include "tracewindow.h"

using namespace Analyzer::Internal;

class QmlProfilerEngine::QmlProfilerEnginePrivate
{
public:
    QmlProfilerEnginePrivate(QmlProfilerEngine *qq) : q(qq) {}
    ~QmlProfilerEnginePrivate() {}

    bool launchperfmonitor();

    QmlProfilerEngine *q;

    QString m_workingDirectory;
    QString m_executable;
    QString m_commandLineArguments;
    Utils::Environment m_environment;

    QProcess *m_process;
    bool m_running;
};

QmlProfilerEngine::QmlProfilerEngine(ProjectExplorer::RunConfiguration *runConfiguration)
    : IAnalyzerEngine(runConfiguration), d(new QmlProfilerEnginePrivate(this))
{
    ProjectExplorer::LocalApplicationRunConfiguration *localAppConfig =
            qobject_cast<ProjectExplorer::LocalApplicationRunConfiguration *>(runConfiguration);

    if (!localAppConfig)
        return;

    d->m_workingDirectory = localAppConfig->workingDirectory();
    d->m_executable = localAppConfig->executable();
    d->m_commandLineArguments = localAppConfig->commandLineArguments();
    d->m_environment = localAppConfig->environment();
    d->m_process = 0;
    d->m_running = false;
}

QmlProfilerEngine::~QmlProfilerEngine()
{
    if (d->m_running)
        stop();
    delete d;
}

void QmlProfilerEngine::start()
{
    d->launchperfmonitor();
    d->m_running = true;

    emit processRunning();
}

void QmlProfilerEngine::stop()
{
    d->m_running = false;
    emit stopRecording();
}

void QmlProfilerEngine::viewUpdated()
{
    d->m_process->terminate();
    if (!d->m_process->waitForFinished(1000)) {
        d->m_process->kill();
        d->m_process->waitForFinished();
    }

    emit processTerminated();
    delete d->m_process;
}

bool QmlProfilerEngine::QmlProfilerEnginePrivate::launchperfmonitor()
{
    bool qtquick1 = false;

    m_process = new QProcess();

    QStringList arguments("-qmljsdebugger=port:" + QString::number(QmlProfilerTool::port) + ",block");
    if (QmlProfilerPlugin::debugOutput)
        qWarning("QmlProfiler: Launching %s", qPrintable(m_executable));

    if (qtquick1) {
        QProcessEnvironment env;
        env.insert("QMLSCENE_IMPORT_NAME", "quick1");
        m_process->setProcessEnvironment(env);
    }

    m_process->setProcessChannelMode(QProcess::ForwardedChannels);
    m_process->setWorkingDirectory(m_workingDirectory);
    m_process->start(m_executable, arguments);

    if (!m_process->waitForStarted()) {
        if (QmlProfilerPlugin::debugOutput)
            qWarning("QmlProfiler: %s failed to start", qPrintable(m_executable));
        return false;
    }

    sleep(1);

    if (QmlProfilerPlugin::debugOutput)
        qWarning("QmlProfiler: Connecting to %s:%d", qPrintable(QmlProfilerTool::host), QmlProfilerTool::port);

    return true;
}
