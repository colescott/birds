import React from "react";
import { connect } from "react-redux";
import { Link } from "react-router";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardHeader, CardText } from "material-ui/Card";

const styles = {
    div: {
        display: "flex",
        flexDirection: "column",
        padding: 10,
    },
    flex: {
        flex: 1,
    }
};

const lessonProgressStyles = {
    complete: {
        color: "green",
        textAlign: "right"
    },
    unstarted: {
        color: "red",
        textAlign: "right"
    },
    inprogress: {
        color: "yellow",
        textAlign: "right"
    },
    missingPrereqs: {
        color: "darkred",
        textAlign: "right"
    }
};

const lessonProgressNames = {
    complete: "Finished",
    unstarted: "Not started",
    inprogress: "In progress",
    missingPrereqs: "Missing prerequisites"
};

const listLessonsInBranch = (props, branch) => {
    return branch.map(function(lesson, i) {
        return <div key={i}>
            <Link to={`/lesson/${lesson.id}`}>
                <CardText>
                    <div style={{ lineHeight: 0 }}>
                        {lesson.title}
                        <div style={lessonProgressStyles[ lesson.progress ]}>
                            {lessonProgressNames[ lesson.progress ]}
                        </div>
                    </div>
                </CardText>
            </Link>
        </div>;
    });
};

const displayBranch = (props, branch) => {
    return (<div style={styles.div}>
        <Card style={styles.flex} >
            <CardHeader
                title={branch.name}
                subtitle={branch.length}
                actAsExpander={true}
                showExpandableButton={true}
            />
            <CardText
            expandable={true} >
            {listLessonsInBranch(props, branch)}
            </CardText>
        </Card>
    </div>);
};

const LessonList = (props) => {
    return (
        <div
            style={{
                dislay: "flux",
                alignItems: "space-around",
                marginTop: 15
            }}
        >
            {props.error && (
                <div style={styles.div}>
                    <Card>
                        <CardHeader
                            title="Error"
                        />
                        <CardText>
                            <p
                                style={{
                                    color: "red"
                                }}
                            >
                                { props.error || ""}
                            </p>
                        </CardText>
                    </Card>
                    <br/>
                </div>
            )}
            {displayBranch(props, props.designBranch)}
            {displayBranch(props, props.manufacturingBranch)}
            {displayBranch(props, props.programmingBranch)}
            {displayBranch(props, props.socialBranch)}
        </div>
    );
};

const mapStateToProps = (state) => ({
    error: s.getLessonListStatus(state).error,
    list: s.getLessonList(state),
    get designBranch() {
        var branch = Object.values(this.list).filter((lesson) => lesson.branch == "design");
        branch.name = "Design";
        branch.map((lesson) => {
            lesson.progress = this.lessonProgress[ lesson.id ] || "unstarted";
            return lesson;
        });
        return branch.sort();
    },
    get manufacturingBranch() {
        var branch = Object.values(this.list).filter((lesson) => lesson.branch == "manufacturing");
        branch.name = "Manufacturing";
        branch.map((lesson) => {
            lesson.progress = this.lessonProgress[ lesson.id ] || "unstarted";
            return lesson;
        });
        return branch.sort();
    },
    get programmingBranch() {
        var branch = Object.values(this.list).filter((lesson) => lesson.branch == "programming");
        branch.name = "Programming";
        branch.map((lesson) => {
            lesson.progress = this.lessonProgress[ lesson.id ] || "unstarted";
            return lesson;
        });
        return branch.sort();
    },
    get socialBranch() {
        var branch = Object.values(this.list).filter((lesson) => lesson.branch == "social");
        branch.name = "Social";
        branch.map((lesson) => {
            lesson.progress = this.lessonProgress[ lesson.id ] || "unstarted";
            return lesson;
        });
        return branch.sort();
    },
    get lessonProgress() {
        var progressArr = s.getUser(state).progress;
        var progressObj = {};
        for (const item in progressArr) {
            progressObj[ progressArr[ item ].id ] = progressArr[ item ].state;
        }
        Object.values(this.list).forEach((lesson) => {
            for (const prereq in lesson.prerequisites) {
                if (progressObj[ prereq.id ] != "complete") {
                    progressObj[ lesson.id ] = "missingPrereqs";
                    return;
                }
            }
        });
        return progressObj;
    },
});

const mapDispatchToProps = (dispatch) => ({
  loadLessons: () => () => dispatch(a.getLessonList()),
});

export default connect(mapStateToProps, mapDispatchToProps)(LessonList);
