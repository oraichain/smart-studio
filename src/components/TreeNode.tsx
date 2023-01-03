import React, { FC, useState } from 'react';

import { Directory, File, FileType, getIconForFileType, Project } from '../models';

interface Props {
  data: File;
  onClick: (file: File) => void;
}

const TreeNode: FC<Props> = ({ data, onClick }) => {
  const [open, setOpen] = useState(true);
  const children = data.type === FileType.Directory ? (data as Directory).children : null;
  const isProject = data instanceof Project;
  const hasChildren = children && children.length > 0;
  return (
    <div
      onClick={(e) => {
        e.stopPropagation();
        if (data.type === FileType.Directory) {
          if (!isProject) return setOpen(!open);
        }
        onClick(data);
      }}
      className={`monaco-tree-row ${hasChildren ? 'has-children' : ''} ${open ? 'expanded' : 'hidden'}`}
      style={{ paddingLeft: isProject ? 0 : 18, cursor: 'pointer' }}
    >
      {!isProject && (
        <div className={`monaco-icon-label ${hasChildren ? 'folder-icon' : getIconForFileType(data.type) + ' file-icon ext-file-icon'}`} title="" style={{ display: 'flex' }}>
          <div className="monaco-icon-label-description-container">
            <a className="label-name">{data.name}</a>
            <span className="label-description" />
          </div>
        </div>
      )}

      {children && children.map((file) => <TreeNode key={file.name} data={file} onClick={onClick} />)}
    </div>
  );
};

export default TreeNode;
