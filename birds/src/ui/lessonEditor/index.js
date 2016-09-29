import React from "react";
import { connect } from "react-redux";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardHeader, CardText } from "material-ui/Card";
import FlatButton from "material-ui/FlatButton";
import TextField from "material-ui/TextField";
import DropDownMenu from "material-ui/DropDownMenu";
import MenuItem from "material-ui/MenuItem";

import ReactMarkdown from "react-markdown";

const styles = {
    div: {
        display: "flex",
        flexDirection: "column",
        padding: 10,
    },
    flex: {
        flex: 1,
        margin: 10,
    }
};

const LessonEditor = (props) => {
    return (
        <Card>
            <Card>
                <CardHeader
                    title="Details"
                />
                <div style={styles.div}>
                    <CardText>
                        <TextField
                            style={styles.flex}
                            floatingLabelText="Lesson Title"
                            type="text"
                            onChange={props.updateTitle()}
                            value={props.getEditor.title || ""}
                        />
                        <TextField
                            style={styles.flex}
                            floatingLabelText="Lesson ID (Leave blank if new)"
                            type="text"
                            onChange={props.updateId()}
                            value={props.getEditor.id || ""}
                        />
                        <DropDownMenu
                            value={props.getEditor.branch || 0}
                            onChange={props.updateBranch()}
                        >
                            <MenuItem value={0} primaryText="Design" />
                            <MenuItem value={1} primaryText="Manufacturing" />
                            <MenuItem value={2} primaryText="Programming" />
                            <MenuItem value={3} primaryText="Social/Business" />
                        </DropDownMenu>
                        <br />
                        <br />
                        <FlatButton label="Create" onClick={props.createLesson()}/>
                        <FlatButton label="Update" onClick={props.updateLesson()}/>
                        <FlatButton label="Load from Remote" onClick={props.loadLesson()}/>
                    </CardText>
                </div>
            </Card>
            <div style={styles.div}>
                <Card zDepth={1} style={styles.flex}>
                    <CardHeader
                        title="Editor"
                    />
                    <CardText>
                        <TextField
                            floatingLabelText="Markdown"
                            type="text"
                            onChange={props.updatePreview()}
                            value={props.getEditor.editor || ""}
                            multiLine={true}
                            rows={10}
                            rowsMax={15}
                            fullWidth={true}
                        />
                    </CardText>
                </Card>
                <Card zDepth={1} style={styles.flex}>
                    <CardHeader
                        title="Preview"
                    />
                    <CardText>
                        <ReactMarkdown source={props.getEditor.editor || ""} />
                    </CardText>
                </Card>
            </div>
        </Card>
    );
};

const mapStateToProps = (state) => ({
    getEditor: s.getLessonEditor(state)
});

const mapDispatchToProps = (dispatch) => ({
    updatePreview: () => (e, value) => dispatch(a.setLessonEditor({ editor: value })),
    updateId: () => (e, value) => dispatch(a.setLessonEditor({ id: value })),
    updateTitle: () => (e, value) => dispatch(a.setLessonEditor({ title: value })),
    updateBranch: () => (e, value) => dispatch(a.setLessonEditor({ branch: value })),
    updateLesson: () => {},
    createLesson: () => (e, value) => dispatch(a.createLesson()),
    loadLesson: () => {}
});

export default connect(mapStateToProps, mapDispatchToProps)(LessonEditor);
