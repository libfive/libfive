#pragma once

#include <QObject>
#include <QPointer>
#include <QVector3D>
#include <QWidget>

class SettingsPane : public QWidget
{
    Q_OBJECT
public:
    SettingsPane(QVector3D min, QVector3D max, float res);
signals:
    void changed(QVector3D min, QVector3D max, float res);
};

////////////////////////////////////////////////////////////////////////////////

class Settings : public QObject
{
    Q_OBJECT
public:
    Settings(QObject* parent=nullptr);
signals:
    void changed(QVector3D min, QVector3D max, float res);

public slots:
    void openPane();
    void applyChange(QVector3D min, QVector3D max, float res);

protected:
    QPointer<SettingsPane> pane;

    QVector3D min=QVector3D(-10, -10, -10);
    QVector3D max=QVector3D(10, 10, 10);
    float res=10;
};
