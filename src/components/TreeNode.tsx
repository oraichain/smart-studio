import React,{FC} from 'react';

import { Directory, File, FileType, getIconForFileType, Project } from '../models';

interface Props  { 
  data: File;
  onClick?: (file: File) => void; 
};

const TreeNode: FC<Props> = ({ data, onClick}) => {
  
  const children= data.type === FileType.Directory ? (data as Directory).children : null;
  const isProject = data instanceof Project
  
  return (
    
      <div onClick={()=>{
        if(data.type !== FileType.Directory) onClick?.(data)
      }} className={`monaco-tree-row ${children?.length ? 'expanded has-children' : ''}`} style={{ paddingLeft: isProject?0:18,cursor:'pointer' }}>
       {!isProject && 
       <div className="content">
          <div className={`monaco-icon-label ${children?.length ? 'folder_type_model folder-icon' :  getIconForFileType(data.type) + ' file-icon'}`} title="" style={{ display: 'flex' }}>
            <div className="monaco-icon-label-description-container">
              <a className="label-name">{data.name}</a>
              <span className="label-description" />
            </div>
          </div>
        </div>}    

      {children?.map((file) => (
        <TreeNode key={file.name} data={file} onClick={onClick} />
      ))}
    </div>
  );
}

export default TreeNode;
