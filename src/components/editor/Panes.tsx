import React from 'react';
import Group from '../../utils/group';
import { resetDOMSelection } from '../../util';
import { splitGroup, openView, closeView, focusTabGroup, setViewType } from '../../actions/AppActions';
import appStore from '../../stores/AppStore';
import { View } from './View';
import { ViewTabs } from './ViewTabs';

export interface EditorPanesProps {
  groups: Group[];
  active: Group;
}

export const EditorPanes = ({ groups, active }: EditorPanesProps) => {
  if (groups.length === 0) {
    return <div>No Groups</div>;
  }
  return (
    <>
      {groups.map((group: Group, i: number) => {
        // tslint:disable-next-line:jsx-key
        return (
          <ViewTabs
            key={`editorPane${i}`}
            views={group.views.slice(0)}
            view={group.currentView}
            preview={group.preview}
            hasFocus={active === group}
            onFocus={() => {
              // TODO: Should be taken care of in shouldComponentUpdate instead.
              focusTabGroup(group);
            }}
            onChangeViewType={(view, type) => setViewType(view, type)}
            onClickView={(view: View) => {
              if (!(appStore.getActiveTabGroup().currentView === view)) {
                // Avoids the propagation of content selection between tabs.
                resetDOMSelection();
              }
              focusTabGroup(group);
              openView(view);
            }}
            onDoubleClickView={(view: View) => {
              focusTabGroup(group);
              openView(view, false);
            }}
            // onClose={(view: View) => {
            //   focusTabGroup(group);
            //   closeView(view);
            // }}
          />
        );
      })}
    </>
  );
};
